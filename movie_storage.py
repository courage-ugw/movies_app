from movie_utils import load_json_file, fetch_movie_data, save_movie_data


def list_movies() -> dict[str: list]:
    """ LOADS THE JSON FILE, AND RETURNS THE DATA IN THE FILE AS DICT """
    movies_data = load_json_file()
    movie_list = {movie: [movies_data[movie]['rating'], movies_data[movie]['year']] for
                  movie in movies_data}
    return movie_list


def add_movie() -> str:
    """ GETS NAME OF MOVIE FROM USER, FETCH DATA FROM API USING THE NAME, LOADS JSON FILE
     AND SAVE SELECTED DATA TO THE FILE. RETURNS SUCCESS MESSAGE """

    movie_name = input("Enter new movie name: ")
    movies_data: dict = fetch_movie_data(movie_name)
    movies_data_json = load_json_file()
    try:
        movies_data_json[movies_data['Title']] = {'year': movies_data['Year'],
                                                  'rating': movies_data['imdbRating'],
                                                  'image_url': movies_data['Poster'],
                                                  'id': movies_data['imdbID'],
                                                  'note': ' ',
                                                  'country': movies_data['Country']}
    except KeyError:
        return f"Movie '{movie_name}' not found"
    save_movie_data(movies_data_json)
    return f"Movie {movie_name} successfully added"


def delete_movie() -> str:
    """ GETS MOVIE NAME TO DELETE FROM THE USER. LOAD DATA FROM THE JSON FILE,
     REMOVE THE MOVIE FROM THE DATA AND SAVE THE DATA AGAIN TO JSON FILE """

    movie_name = input("Enter movie name to delete: ")
    movies_data = load_json_file()
    for movie in movies_data:
        if movie.lower() == movie_name.lower():
            movies_data.pop(movie)
            save_movie_data(movies_data)
            return f"Movie {movie_name} successfully deleted"
    else:
        save_movie_data(movies_data)
        return f"Movie {movie_name} doesn't exist!"


def update_movie() -> str:
    """ GETS MOVIE NAMES AND NOTE FROM USER. LOADS DATA FROM JSON FILE. FINDS THE
     MOVIE IN DATA AND UPDATE THE DATA WITH THE NEW NOTE FROM THE USER."""
    movie_name = input("Enter movie name: ")
    movie_note = input("Enter movie note: ").title()
    movies_data = load_json_file()
    for name in movies_data:
        if name.lower() == movie_name.lower():
            movies_data[name].update({'note': movie_note})
            save_movie_data(movies_data)
            return f"Movie {movie_name} successfully updated"
    return f"Movie '{movie_name}' doesn't exist!"