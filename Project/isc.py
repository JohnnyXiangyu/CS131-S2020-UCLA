connections = {
    'Singleton': ['Cambpell', 'Jaques', 'Smith'],
    'Cambpell': ['Singleton'],
    'Jaques': ['Singleton', 'Hill'],
    'Smith': ['Singleton', 'Hill'],
    'Hill': ['Jaques', 'Smith']
}

port_numbers = {
    'Singleton': 12405,
    'Cambpell': 12406,
    'Jaques': 12407,
    'Smith': 12408,
    'Hill': 12409
}

def checkName(name):
    """
    validity check of server name

    Arguments:
        name {str} -- server name from input
    """
    if name in ['Singleton', 'Cambpell', 'Jaques', 'Smith', 'Hill']:
        return name
    else:
        return False
