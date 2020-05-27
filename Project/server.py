import argparse
import asyncio
import isc
import dev_flags
import re
import time

# re matchers
f_iamat = re.compile(
    'IAMAT .+ [+-][0-9]+\.[0-9]+[+-][0-9]+\.[0-9]+ [0-9]+\.[0-9]+')
f_at = re.compile(
    'AT (Hill|Combpell|Jaques|Smith|Singleton) [+-][0-9]+\.[0-9]+ .+ [+-][0-9]+\.[0-9]+[+-][0-9]+\.[0-9]+ [0-9]+\.[0-9]+')
f_whatsat = re.compile('WHATSAT .+ [0-9]+ [0-9]+')


class Server:
    """
    server object
    """

    def __init__(self, name, ip='127.0.0.1', message_max_length=1e6):
        """
        given name, generate a CS131 project server

        Arguments:
            name {str} -- pass processed name

        Keyword Arguments:
            ip {str} -- ip address of server (default: {'127.0.0.1'})
            message_max_length {[type]} -- don't change this value, it's maximum length of a single read (default: {1e6})
        """
        self.name = name
        self.ip = ip
        self.port_number = isc.port_numbers[name]
        self.neighbours = isc.connections[name]
        self.message_max_length = int(message_max_length)
        self.database = {}

    async def broadcast(self, message="", exclusion=[]):
        """
        send message to all neighbours, except the sender if it's server

        Keyword Arguments:
            message {str} -- the message, finalized, to be sent to all servers (default: {""})
            exclusion {list} -- don't send to these neighbours (default: {[]})
        """
        # open connection to each neighbour server, send the message
        if dev_flags.debug:
            print('Flooding message.')
        for neighbour in self.neighbours:
            try:
                if neighbour in exclusion:  # exclude some servers
                    if dev_flags.debug:
                        print(f'  Skip sender: {neighbour}')
                    continue
                n_ip = '127.0.0.1'
                n_port = isc.port_numbers[neighbour]
                reader, writer = await asyncio.open_connection(n_ip, n_port)
                if dev_flags.debug:
                    print(f'  Send message to {neighbour}: \n\t{message}')
                writer.write(message.encode())
                await writer.drain()  # is this line necessary here?
                if dev_flags.debug:
                    print(f'  Close socket to {neighbour}')
                writer.close()

            # handle server-down scenario
            except ConnectionRefusedError as e:
                if dev_flags.debug:
                    print(f'  Server {neighbour} is down, skipped. Original error:\n\t{e}')
                continue

    def getLongLat(self, loc_text=""):
        """
        input formatted longitudinal and latitudinal information, return dict with keywords 'long' 'lat'
        sample input:
            +34.068930-118.445127
        input text should always be well formatted, checked by filter message

        Keyword Arguments:
            loc_text {str} -- +-long+-lat (default: {""})
        """
        longitude = float(re.search(
            '([+-].*)[+-].*', loc_text).group(1))  # might need to handle exception
        latitude = float(re.search('[+-].*([+-].*)', loc_text).group(1))
        return {'long': longitude, 'lat': latitude}

    def parseMessage(self, message="", type=""):
        """
        parse message into dictionary
        AT and IAMAT have same structure after heading
            [AT] type, serv_name, time_diff, client_name, long, lat, time
            [IAMAT] type, client_name, long, lat, time
            [WHATSAT] type, client_name, radius, count

        Keyword Arguments:
            message {str} -- message to be parsed (default: {""})
            type {str} -- type of the message (default: {""})
        """
        # AT Hill +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        # IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        # WHATSAT kiwi.cs.ucla.edu 10 5 # TODO: figure out if radius must be integer or not

        words = message.split()  # split message into words
        parse = {'type': type}
        if type == 'AT':
            parse.update({
                'serv_name': words[1],
                'time_diff': float(words[2]),
                'client_name': words[3],
                'time': float(words[5])
            })
            parse.update(self.getLongLat(words[4]))
        elif type == 'IAMAT':
            parse.update({'client_name': words[1], 'time': float(words[3])})
            parse.update(self.getLongLat(words[2]))  # return 2 keywords
        elif type == 'WHATSAT':
            parse.update({
                'client_name': words[1],
                'radius': int(words[2]),
                'count': int(words[3])
            })
            if parse['radius'] > 50 or parse['radius'] < 0 or parse['radius'] > 20 or parse['radius'] < 0:
                parse.update({'type': 'ERROR'})

        return parse

    def filterMessage(self, message=""):
        """
        return type of input in string format, keyword list:

        Keyword Arguments:
            message {str} -- message to be examined (default: {""})
        """
        # use re to check string format
        incoming_type = ""
        if f_at.match(message):
            incoming_type = 'AT'
        elif f_iamat.match(message):
            incoming_type = 'IAMAT'
        elif f_whatsat.match(message):
            incoming_type = 'WHATSAT'
        else:
            return False
        if dev_flags.debug:
            print(
                f'incoming transmission passes grammar check:\n\t{incoming_type}')

        # parse incoming message and check value sanity
        data = self.parseMessage(message, incoming_type)
        if data['type'] == 'ERROR':
            return False
        else:
            return data

    def updateDatabase(self, name, time, long, lat):
        """
        update database with new incoming message (or create new record)

        Arguments:
            name {str} -- name of client
            time {num} -- unix timestamp of when this message is sent
            long {num} -- longtitude of client
            lat {num} -- latitude of client
        """
        try:
            old_record = self.database[name]
            if time > old_record['time']:
                self.database.update(
                    {name: {'time': time, 'long': long, 'lat': lat}})
                return True
        except KeyError:
            self.database.update(
                {name: {'time': time, 'long': long, 'lat': lat}})
            return True
        return False

    async def serve_client(self, reader, writer):
        """
        server routine,
        handle incoming messages:
            IAMAT
            AT
            WHATSAT
        broadcast messages to other servers

        """
        # read in message and check its type
        msg_raw = await reader.read(self.message_max_length)
        msg = msg_raw.decode()
        incoming_data = self.filterMessage(msg)  # here a dict is returned

        if incoming_data == False:  # if message format is wrong
            err_response = f'? {msg}'
            writer.write(err_response.encode())
            await writer.drain()

        elif incoming_data['type'] == 'IAMAT':
            # respond to client
            msg_back = 'AT ' + self.name + \
                ' {0:+}'.format(time.time() - incoming_data['time']) + msg[5:]
            writer.write(msg_back.encode())
            await writer.drain()

            # update database and start a flooding (only when update takes place)
            if self.updateDatabase(
                incoming_data['client_name'], incoming_data['time'], incoming_data['long'], incoming_data['lat']):
                await self.broadcast(msg_back, exclusion=[])

        elif incoming_data['type'] == 'AT':
            # update database broadcast to all except incoming neighbour if update takes place
            if self.updateDatabase(
                incoming_data['client_name'], incoming_data['time'], incoming_data['long'], incoming_data['lat']):
                await self.broadcast(msg, exclusion=[incoming_data['serv_name']])
            else:
                if dev_flags.debug:
                    print("Skipping old message.")

        elif incoming_data['type'] == 'WHATSAT':
            # TODO: query google place API
            # TODO: send back the JSON response
            pass
        writer.close()
        if dev_flags.debug:
            print(self.database)

    async def run_forever(self):
        """
        open server on port (specified by name)
        """
        server_instance = await asyncio.start_server(self.serve_client, self.ip, self.port_number)
        print(f'server {self.name} starts on localhost:{self.port_number}')
        async with server_instance:
            await server_instance.serve_forever()
        server_instance.close()


def main():
    """
    the main routine for testing servers only
    """
    parser = argparse.ArgumentParser(
        'launch server.py with a valid server name')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    if dev_flags.debug:
        print("argument processed \nserver name: {}".format(args.server_name))

    # check name validity
    server = None
    name = isc.checkName(args.server_name)
    if name:
        server = Server(name)
    else:
        print('Error: invalid server name "{}"'.format(args.server_name))
        exit(114)

    # open server on port, close only on SIGINT
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()
