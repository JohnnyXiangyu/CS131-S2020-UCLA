connections = {
    'Singleton': ['Campbell', 'Jaquez', 'Smith'],
    'Campbell': ['Singleton', 'Smith'],
    'Jaquez': ['Singleton', 'Hill'],
    'Smith': ['Singleton', 'Hill', 'Campbell'],
    'Hill': ['Jaquez', 'Smith']
}

port_numbers = {
    'Singleton': 12405,
    'Campbell': 12406,
    'Jaquez': 12407,
    'Smith': 12408,
    'Hill': 12409
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
