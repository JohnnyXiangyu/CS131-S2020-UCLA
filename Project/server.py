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
        for neighbour in self.neighbours:
            try:
                if neighbour in exclusion:  # exclude some servers
                    continue
                n_ip = '127.0.0.1'
                n_port = isc.port_numbers[neighbour]
                reader, writer = await asyncio.open_connection(n_ip, n_port)
                if dev_flags.debug:
                    print(f'send message to {neighbour}: \n\t{message}')
                writer.write(message.encode())
                await writer.darin()  # is this line necessary here?
                if dev_flags.debug:
                    print(f'close socket to {neighbour}')
                writer.close()

            # handle server-down scenario
            except ConnectionRefusedError:
                continue

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
        # TODO: parse messages
        if type == 'AT':
            pass
        elif type == 'IAMAT':
            pass
        elif type == 'WHATSAT':
            pass

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
            print(f'incoming transmission:\n\t{incoming_type}')

        data = self.parseMessage(message, incoming_type)
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
        except KeyError:
            self.database.update(
                {name: {'time': time, 'long': long, 'lat': lat}})

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
        response = ""
        incoming_data = self.filterMessage(msg)  # here a dict is returned
        if incoming_data == False:
            response = f'? {msg}'

        if incoming_data['type'] == 'IAMAT':
            # update database
            self.updateDatabase(
                incoming_data['client_name'], incoming_data['time'], incoming_data['long'], incoming_data['lat'])

            # respond to client
            msg_back = 'AT ' + self.name + \
                str(time.time() - incoming_data['time']) + msg[2:]
            writer.write(msg_back.encode())
            await writer.drain()

            # broadcast to all neighbours
            await self.broadcast(msg_back, exclusion=[])

        elif incoming_data['type'] == 'AT':
            # update database
            self.updateDatabase(
                incoming_data['client_name'], incoming_data['time'], incoming_data['long'], incoming_data['lat'])

            # broadcast to all except incoming neighbour
            await self.broadcast(msg, exclusion=[incoming_data['serv_name']])

        elif incoming_data['type'] == 'WHATSAT':
            # TODO: query google place API
            # TODO: send back the JSON response
            pass

    async def run_forever(self):
        """
        open server on port (specified by name)
        """
        server_instance = await asyncio.start_server(self.serve_client, self.ip, self.port_number)
        print(f'server {self.name} starts on localhost:{self.port_number}')
        async with server_instance:
            await server_instance.serve_forever
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
        print("argument processed \n\tserver name: {}".format(args.server_name))

    # check name validity
    server = None
    name = isc.checkName(args.server_name)
    if name:
        server = Server(name)
    else:
        print('Error: invalid server name "{}"'.format(args.server_name))

    # open server on port, close only on SIGINT
    server.run_forever()


if __name__ == '__main__':
    main()
