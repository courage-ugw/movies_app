from fuzzywuzzy import process
from movie_storage import list_movies, add_movie, update_movie, delete_movie
import logging
import random
import re
import statistics
import requests
import json

# Movie Database Endpoint (API)
REQUEST_URL = f'https://www.omdbapi.com/?apikey=eb462b7d&t='

# Country Flag API
COUNTRY_FLAG_REQUEST_URL = "https://countriesnow.space/api/v0.1/countries/flag/images"

# JSON FILE PATH
MOVIES_DATA_FILE = 'data.json'

# Initial movie data to set up JSON file
INITIAL_MOVIE_NAMES = ['The Room', 'Titanic', 'Pulp Fiction']

# Movie App Title.
APP_TITLE = "\n** ** ** ** ** My Movies Database ** ** ** ** **"

# Placeholder in the HTML Template file. The placeholder is replaced if exist.
HTML_TEMPLATE_TITLE = "Masterschool's Movie App"

# File path for the HTML Template FIle
HTML_TEMPLATE_FILE_PATH = '_static/index.html'

# REGEX pattern for HTML content
SEARCH_PATTERN = '<li>[\s\S]*?<div class="movie">\s*?<a href=".*?" target="_blank">' \
                 '\s*?<img class="movie-poster" src=".*?" title=""><\/a>\s*?' \
                 '<div class="movie-title">.*?<\/div>\s*?<div class="movie-year">.*?' \
                 '<\/div>[\s\S]*?<\/li>'

# MENU: Displays Menu options to the user
USER_CHOICE_STR: str = """ 
Menu:
0. Exit
1. List movies
2. Add movie
3. Delete movie
4. Update movie
5. Stats
6. Random movie
7. Search movie
8. Movies sorted by rating
9. Generate website

Enter choice (0-9): 
"""
# Dispatch Table: {menu option: Func}
USER_CHOICE_DICT = {1: 'list_movies',
                    2: 'add_movie',
                    3: 'delete_movie',
                    4: 'update_movie',
                    5: 'movie_stats',
                    6: 'random_movie',
                    7: 'search_movies',
                    8: 'movies_sorted_by_rating',
                    9: 'generate_website',
                    }

# If user input is 0, then user exits the app
EXIT_APP = 0


def app_start() -> None:
    """ DISPLAYS THE MAIN MENU, GETS USER INPUT, CALLS THE REQUIRED FUNCTION,
    EXITS APP WHEN USER QUITS """

    # CREATES JSON FILE IF FILE DOES NOT EXIT
    create_file()

    # GETS USER INPUT AND VALIDATE INPUT
    user_input = get_user_input()
    user_input_validated = validate_user_input(user_input)

    while user_input_validated is not EXIT_APP:
        # CALLS THE FUNCTION TO EXECUTE USER CHOICE AND DISPLAYS MESSAGE TO THE USER
        dispatcher = eval(USER_CHOICE_DICT[int(user_input_validated)])
        output = dispatcher(load_json_file, save_movie_data, fetch_movie_data)
        display_message(output)

        # PROMPTS USER TO PRESS ENTER AND CHECKS THAT USER PRESSES THE 'ENTER' KEY
        user = input("\nPress Enter to continue ")
        while user:
            user = input("\nPress Enter to continue ")

        # GETS USER INPUT AND VALIDATE INPUT
        user_input = input(USER_CHOICE_STR)
        user_input_validated = validate_user_input(user_input)

    # DISPLAY MESSAGE WHEN USER EXITS  APP
    print('Bye!')


def get_user_input():
    return input(USER_CHOICE_STR)


def is_digit(user_input):
    while not user_input.isdigit():
        user_input = get_user_input()
    return user_input


def validate_user_input(user_input):
    user_input = is_digit(user_input)
    while int(user_input) not in range(10):
        print(f"The option '{int(user_input)}' is not in the menu.")
        new_user_input = get_user_input()
        user_input = is_digit(new_user_input)
    return user_input


def display_message(output: str) -> None:
    """ DISPLAYS MESSAGES TO THE USER AFTER USER CHOICE ACTION HAS BEEN PERFORMED """
    if isinstance(output, str):
        print(output)
    elif isinstance(output, dict):
        print(f'{len(output)} movies in total: ')
        for item in output:
            print(f"{item}: {output[item][0]}, {output[item][1]}")
    else:
        for item in output:
            print(f'{item[0]}: {item[1]}')


def create_file():
    """ CREATES THE JSON FILE, IF THE FILE ALREADY EXISTS, DO NOTHING """
    try:
        with open(MOVIES_DATA_FILE, 'x') as file:
            json.dump({}, file)  # INITIALIZE THE FILE WITH AN EMPTY DICTIONARY
        initialize_json_file(INITIAL_MOVIE_NAMES)
    except FileExistsError:
        pass


def load_json_file() -> dict:
    """ LOADS THE MOVIES FROM THE JSON FILE """
    with open(MOVIES_DATA_FILE, 'r') as json_file:
        return json.load(json_file)


def save_movie_data(movie_data: str or dict) -> None:
    """ SAVES GIVING MOVIE DATA TO FILE """
    with open(MOVIES_DATA_FILE, 'w') as json_file:
        json.dump(movie_data, json_file)


def load_html_file() -> str:
    """ LOADS HTML FILE AND RETURNS THE CONTENT AS STRING """
    with open(HTML_TEMPLATE_FILE_PATH, 'r') as html_file:
        return html_file.read()


def fetch_movie_data(movie_name: str or list) -> dict:
    """ FETCH MOVIE DATA USING 'NAME'.
     IF JSON FILE IS EMPTY: INITIALIZE JSON FILE USING A LIST OF NAMES """

    # Fetches Movie data to initialize Json file using movie names in list:
    # ['The Room', 'Titanic', 'Pulp Fiction']
    if type(movie_name) is list:
        movies_data = {}
        for each_movie_name in movie_name:
            url = REQUEST_URL + each_movie_name
            movies = requests.get(url).json()
            movies_data[each_movie_name] = movies
        return movies_data

    # Fetch Movie Data using movie_name
    url = REQUEST_URL + movie_name
    movies_data = requests.get(url).json()
    return movies_data


def initialize_json_file(movie_name: list):
    """
    LOADS JSON FILE.
    INITIALIZES THE JSON FILE WITH INITIAL DATA AND SAVES IT.
    """
    movies_data_json = load_json_file()
    movies_data_dict = fetch_movie_data(movie_name)
    movies_data = movies_data_dict.values()
    for movie_data in movies_data:
        movies_data_json[movie_data['Title']] = {'year': movie_data['Year'],
                                                 'rating': movie_data['imdbRating'],
                                                 'image_url': movie_data['Poster'],
                                                 'id': movie_data['imdbID'], 'note': ' ',
                                                 'country': movie_data['Country']}
    save_movie_data(movies_data_json)


def get_img_element(country):
    payload_json = {"country": country.strip()}
    response = requests.post(COUNTRY_FLAG_REQUEST_URL, json=payload_json)
    if not response.json()['error']:
        return f'<img class="country-flag" src="{response.json()["data"]["flag"]}" ' \
               f'alt="{country.strip()}">'
    else:
        return f'<img class="country-flag" src="{country.strip()}" ' \
               f'alt="{country.strip()}">'


def get_country_flag(country_name: str) -> str:
    """ GETS THE COUNTRY NAME.
        IF TWO COUNTRY NAMES EXIST, SPLIT NAMES INTO EACH COUNTRY NAME.
        FETCH THE COUNTRY FLAG FOR EACH COUNTRY.
        RETURNS HTML IMG TAG CONTAINING THE FLAG URL """

    country_flag_output = ''
    # If more than one country exist, generate Html img element for each country
    if ',' in country_name:
        country_names = country_name.split(',')
        for country in country_names:
            country_flag_output += get_img_element(country)
    else:
        # Generates HTML img element for only one country
        country_flag_output += get_img_element(country_name)
    return country_flag_output


def movie_stats(*args) -> str:
    """ LOADS JSON FILE, GETS MOVIE NAMES AND RATINGS, CALLS THE CALCULATE MOVIES
     FUNCTION AND RETURNS MOVIE STATS """

    movies_data = load_json_file()
    movies_name_ratings = movie_names_and_ratings(movies_data)
    return calculate_movie_stats(movies_name_ratings)


def calculate_movie_stats(movies_name_and_ratings: dict) -> str:
    """ GETS MOVIES DATA AS DICT, CALCULATES MEAN AND MEDIAN STATISTICS ON THE DATA USING
    THE RATINGS AND RETURNS THE MEAN, MEDIAN AND BEST MOVIES BASED RATINGS """
    movie_ratings = movies_name_and_ratings.values()
    movie_ratings = [float(ratings) for ratings in movie_ratings]
    average_rating = statistics.mean(movie_ratings)
    median_rating = statistics.median(movie_ratings)
    movies_sorted = sorted(movies_name_and_ratings.items(), key=lambda item: item[1],
                           reverse=True)
    best_movie, best_rating = movies_sorted[0]
    worst_movie, worst_rating = movies_sorted[-1]
    return f"""
Average rating: {average_rating :.2f}
Median rating: {median_rating}
Best Movie: {best_movie}, {best_rating}
Worst Movie: {worst_movie}, {worst_rating}
"""


def random_movie(*args) -> str:
    """ LOAD DICT DATA FROM JSON FILE, GETS ONLY MOVIE NAMES AND RATINGS, USE
     THE RANDOM FUNCTION TO GET A RANDOM MOVIE NAME AND RATING AND RETURN THEM """

    movies_data = load_json_file()
    movies_name_ratings = movie_names_and_ratings(movies_data)
    movie, rating = random.choice(list(movies_name_ratings.items()))
    return f"Your movie for tonight: {movie}, it's rated {rating}"


def search_movies(*args) -> str:
    """ GETS MOVIE NAME OF OR PART OF THE MOVIE NAME TO SEARCH FOR, APPLIES NAME
     COMPARISON WITH FUZZYWUZZY MODULE, AND RETURNS THE MOVIE NAME THAT IS BEING
     SEARCHED FOR ALONGSIDE ITS RATING """

    # suppresses warning from fuzzywuzzy library if input don't contain any common
    # characters of a natural language
    logging.getLogger().setLevel(logging.ERROR)

    movie_name = input('Enter part of movie name: ')
    movies_data = load_json_file()
    movies_name_ratings = movie_names_and_ratings(movies_data)
    search_result = process.extractOne(movie_name, movies_name_ratings.keys(),
                                       score_cutoff=70)
    if search_result:
        for movie, rating in movies_name_ratings.items():
            if search_result[0].lower() == movie.lower():
                return f"{movie.title()}, {rating}"
    return f'The string "{movie_name}" does not match any movie name!'


def movie_names_and_ratings(movies_data: dict) -> dict:
    """ GETS MOVIES DICT DATA, RETURNS ONLY MOVIE NAMES AND RATINGS """
    movies = {movie: movies_data[movie]['rating'] for movie in movies_data}
    return movies


def movies_sorted_by_rating(*args) -> list:
    """ LOADS DICT DATA FROM JSON FILE, GETS ONLY THE MOVIE NAMES AND
    THEIR RATINGS, SORTS DATA BASED ON RATINGS. RETURNS THE SORTED DATA AS LIST """
    movies_data = load_json_file()
    movies = movie_names_and_ratings(movies_data)
    movies_sorted = sorted(movies.items(), key=lambda item: item[1], reverse=True)
    return movies_sorted


def generate_website(*args) -> str:
    """ LOADS CONTENT FROM HTML FILE, LOADS MOVIES DATA FROM JSON FILE AND SERIALIZES
     THE DATA FOR HTML. REPLACES THE HTML FILE WITH THE SERIALIZED DATA """

    html_content = load_html_file()
    movies_data = load_json_file()
    movies_data_serialized = serialize_movie_data(movies_data)
    return replace_html_content(movies_data_serialized, html_content)


def serialize_movie_data(movies_data: dict) -> str:
    """ GETS MOVIES DATA, SERIALIZES THE DATA FOR HTML """
    movies_html_output = ''
    for movie in movies_data:
        # Don't display image if no poster url
        if movies_data[movie]["image_url"].startswith('https'):
            # opening tag
            movies_html_output += '<li>\n\t<div class="movie">\n'
            # Movie title
            movies_html_output += '\t<a href="https://www.imdb.com/title/' \
                                  f'{movies_data[movie]["id"]}/" target="_blank">\n'
            # Movie poster url
            movies_html_output += '\t<img class="movie-poster" src="' \
                                  f'{movies_data[movie]["image_url"]}" title=""></a>\n'
            # Movie rating
            movies_html_output += f'\t<div class="movie-title">{movie} ' \
                                  f'[{movies_data[movie]["rating"]}]</div>\n'
            # Movie year
            movies_html_output += '\t<div class="movie-year">' \
                                  f'{movies_data[movie]["year"]}</div>'
            # Movie country flag
            movies_html_output += '\n<div class="movie-country">'
            movies_html_output += f'\n{get_country_flag(movies_data[movie]["country"])}</div>'
            # Movie notes
            movies_html_output += '\n\t<div class="notes">' \
                                  f'{movies_data[movie]["note"]}</div>'
            # closing tags
            movies_html_output += '\n</div>\n</li>'
    return movies_html_output


def replace_html_content(html_serialized_output: str, html_content: str) -> str:
    """
    REPLACES PART OF THE ORIGINAL HTML CONTENT WITH THE SERIALIZED HTML OUTPUT,
    SAVES THE OUTPUT TO HTML FILE
    """

    # Replaces __TEMPLATE_TITLE__ and __TEMPLATE_MOVIE_GRID__ placeholders
    if '__TEMPLATE_TITLE__' and '__TEMPLATE_MOVIE_GRID__' in html_content:
        html_content_replaced = html_content \
            .replace('__TEMPLATE_TITLE__', HTML_TEMPLATE_TITLE).replace(
                                        '__TEMPLATE_MOVIE_GRID__', html_serialized_output)
    else:
        searched_output = search_html_content(html_content)
        html_content_replaced = html_content.replace(searched_output,
                                                     html_serialized_output)
    save_html_file(html_content_replaced)
    return 'successfully generated the website'


def search_html_content(html_content: str) -> str:
    """ SEARCH FOR THE SPECIFIED PATTERN IN THE HTML FILE CONTENT.
      RETURNS THE SEARCHED OUTPUT AS A STRING """
    searched_string = ''
    searched_output = re.findall(SEARCH_PATTERN, html_content)
    for output in searched_output:
        searched_string += output
    return searched_string


def save_html_file(html_content: str) -> None:
    """ SAVES THE HTML TO HTML FILE """
    with open(HTML_TEMPLATE_FILE_PATH, 'w') as html_file:
        html_file.write(html_content)


def main():
    """ PRINTS THE APP NAME, AND CALLS THE MAIN MENU FUNCTION """
    print(APP_TITLE)
    app_start()


if __name__ == "__main__":
    main()
