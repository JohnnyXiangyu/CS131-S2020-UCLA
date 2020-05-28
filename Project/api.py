import asyncio
import aiohttp

API_KEY = 'AIzaSyBBGS9atYkz0hk61GHmlVewlq5ziXoWpSo'

async def fetchPlaces(lat='+1', lon='-1', radius=1):
    """
    make a single request to google places server and return the dict response

    Keyword Arguments:
        lat {str} -- latitude of search center (default: {'+1'})
        lon {str} -- longitude of search center (default: {'-1'})
        radius {int} -- radius of search (default: {1})
    """
    url = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={API_KEY}&location={lat},{lon}&radius={radius}'
    async with aiohttp.ClientSession(
        connector=aiohttp.TCPConnector(
            ssl=False,
        ),
    ) as session:
        async with session.get(url) as resp:
            response = await resp.json()
            return response
