import dev_flags
import time

class Logger:
    '''
    logger object
    '''
    def __init__(self, server_name):
        """file open"""
        self.fileName = server_name + f'_{int(time.time())}' + '.log'
        self.m_file = open(self.fileName, 'w')
        if dev_flags.debug:
            print(f'Logger: {self.fileName} created.\n')

    def printFile(self, data=''):
        """
        log a line in logfile, print the line on console in debug mode

        Arguments:
            data {str} -- a line of log
        """
        self.m_file.write(data+'\n')
        if dev_flags.debug:
            print(data)
    
    def close(self):
        self.m_file.close()
        if dev_flags.debug:
            print(f'Logger: {self.fileName} closed.\n')
    
    def __del__(self):
        self.close()
