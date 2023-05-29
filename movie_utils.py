import requests
import json

API_KEY = 'eb462b7d'
REQUEST_URL = f'https://www.omdbapi.com/?apikey={API_KEY}&t='
COUNTRY_FLAG_REQUEST_URL = "https://countriesnow.space/api/v0.1/countries/flag/images"
MOVIES_DATA_FILE = 'data.json'  # JSON FILE PATH
# NAMES USED TO INITIALIZE DATA IN FILE
INITIAL_MOVIE_NAMES = ['The Room', 'Titanic', 'Pulp Fiction']


def create_file() -> None:
    """ CREATES THE JSON FILE, IF THE FILE ALREADY EXISTS, DO NOTHING """
    try:
        with open(MOVIES_DATA_FILE, 'x') as file:
            json.dump({}, file)  # INITIALIZE THE FILE WITH AN EMPTY DICTIONARY
        add_data_to_file(INITIAL_MOVIE_NAMES)
    except FileExistsError:
        pass


def add_data_to_file(movie_name: list) -> None:
    """ LOADS THE JSON FILE, FETCH DATA FROM API USING A LIST OF NAMES, INITIALIZES THE
    FILE WITH THE DATA """
    movies_data_json = load_json_file()
    movies_data_dict: dict = fetch_movie_data(movie_name)
    movies_data = movies_data_dict.values()
    for movie_data in movies_data:
        movies_data_json[movie_data['Title']] = {'year': movie_data['Year'],
                                                 'rating': movie_data['imdbRating'],
                                                 'image_url': movie_data['Poster'],
                                                 'id': movie_data['imdbID'],
                                                 'note': ' ',
                                                 'country': movie_data['Country']}
    save_movie_data(movies_data_json)


def fetch_movie_data(movie_name: str or list) -> dict:
    """FETCH MOVIE DATA USING NAME OR LIST OF NAMES AND RETURNS THE DATA AS DICTIONARY """
    if type(movie_name) is list:
        movies_data = {}
        for each_movie_name in movie_name:
            url = REQUEST_URL + each_movie_name
            movies = requests.get(url).json()
            movies_data[each_movie_name] = movies
        return movies_data
    url = REQUEST_URL + movie_name
    movies_data = requests.get(url).json()
    return movies_data


def load_json_file() -> dict:
    """ LOADS THE MOVIES FROM THE JSON FILE """
    with open(MOVIES_DATA_FILE, 'r') as json_file:
        return json.load(json_file)


def save_movie_data(movie_data: str or dict) -> None:
    """ SAVES GIVING MOVIE DATA TO FILE """
    with open(MOVIES_DATA_FILE, 'w') as json_file:
        json.dump(movie_data, json_file)


def get_country_flag(country_name: str) -> str:
    """ THIS FUNCTION GETS THE COUNTRY NAME AS STRING AND CHECKS IF THE STRING
     HAS MORE THAN ONE COUNTRY NAME. IT SPLITS THE STRING INTO THE DIFFERENT COUNTRY
     NAMES AND FETCHES THE COUNTRY FLAG FOR EACH COUNTRY. RETURNS HTML IMG TAG
     CONTAINING THE FLAG URL """

    country_flag_output = ''
    if ',' in country_name:
        country_names = country_name.split(',')
        for country in country_names:
            payload_json = {"country": country.strip()}
            response = requests.post(COUNTRY_FLAG_REQUEST_URL, json=payload_json)
            country_flag_output += '<img class="country-flag" ' \
                                   f'src="{response.json()["data"]["flag"]}" ' \
                                   f'alt="{country.strip()}">'
    else:
        # GENERATES HTML IMG TAG FOR ONLY ONE COUNTRY
        payload_json = {"country": country_name.strip()}
        response = requests.post(COUNTRY_FLAG_REQUEST_URL, json=payload_json)
        country_flag_output += '<img class="country-flag" ' \
                               f'src="{response.json()["data"]["flag"]}" ' \
                               f'alt="{country_name.strip()}">'
    return country_flag_output
