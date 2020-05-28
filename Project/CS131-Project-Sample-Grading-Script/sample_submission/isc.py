connections = {
    'Singleton': ['Campbell', 'Jaquez', 'Smith'],
    'Campbell': ['Singleton', 'Smith'],
    'Jaquez': ['Singleton', 'Hill'],
    'Smith': ['Singleton', 'Hill', 'Campbell'],
    'Hill': ['Jaquez', 'Smith']
}

port_numbers = {
    'Singleton': 8000,
    'Campbell': 8001,
    'Jaquez': 8002,
    'Smith': 8003,
    'Hill': 8004
}

def checkName(name):
    """
    validity check of server name

    Arguments:
        name {str} -- server name from input
    """
    if name in ['Singleton', 'Campbell', 'Jaquez', 'Smith', 'Hill']:
        return name
    else:
        return False
