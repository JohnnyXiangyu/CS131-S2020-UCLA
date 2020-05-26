import argparse
import dev_flags
from server import Server

def main():
    parser = argparse.ArgumentParser('launch server.py with a valid server name')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    if dev_flags.debug:
        print("argument processed \n\tserver name: {}".format(args.server_name))
    
    # TODO: check name validity
    # TODO: open server on port


if __name__ == '__main__':
    main()
