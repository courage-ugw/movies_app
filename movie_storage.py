def list_movies(*args) -> dict[str: list]:
    """ LOADS THE JSON FILE, AND RETURNS THE DATA IN THE FILE AS DICT """
    load_json_file, *_ = args
    movies_data = load_json_file()
    movie_list = {movie: [movies_data[movie]['rating'], movies_data[movie]['year']] for
                  movie in movies_data}
    return movie_list


def add_movie(*args) -> str:
    """ GETS NAME OF MOVIE FROM USER, FETCH DATA FROM API USING THE NAME, LOADS JSON FILE
     AND SAVE SELECTED DATA TO THE FILE. RETURNS SUCCESS MESSAGE """

    load_json_file, save_movie_data, fetch_movie_data = args
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


def delete_movie(*args) -> str:
    """ GETS MOVIE NAME TO DELETE FROM THE USER. LOAD DATA FROM THE JSON FILE,
     REMOVE THE MOVIE FROM THE DATA AND SAVE THE DATA AGAIN TO JSON FILE """

    load_json_file, save_movie_data, _ = args
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


def update_movie(*args) -> str:
    """ GETS MOVIE NAMES AND NOTE FROM USER. LOADS DATA FROM JSON FILE. FINDS THE
     MOVIE IN DATA AND UPDATE THE DATA WITH THE NEW NOTE FROM THE USER."""

    load_json_file, save_movie_data, _ = args
    movie_name = input("Enter movie name: ")
    movie_note = input("Enter movie note: ").title()
    movies_data = load_json_file()
    for name in movies_data:
        if name.lower() == movie_name.lower():
            movies_data[name].update({'note': movie_note})
            save_movie_data(movies_data)
            return f"Movie {movie_name} successfully updated"
    return f"Movie '{movie_name}' doesn't exist!"