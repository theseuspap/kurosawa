import pandas as pd
print(pd.__version__)

city_names = pd.Series(['San Francisco', 'San Jose', 'Sacramento'])
population = pd.Series([852469, 1015785, 485199])

cities_population_dataframe = pd.DataFrame({ 'City name': city_names, 'Population': population }).describe()
california_housing_dataframe = pd.read_csv("https://download.mlcc.google.com/mledu-datasets/california_housing_train.csv", sep=",")

#print(california_housing_dataframe.describe())
#print(cities_population_dataframe.describe())
#print(california_housing_dataframe.head())

cities = pd.DataFrame({ 'City name': city_names, 'Population': population })
#print(cities['City name'])
#print(cities['Population'])

cities['Area square miles'] = pd.Series([46.87, 176.53, 97.92])
cities['Is wide and has saint name'] = (cities['Area square miles'] > 50) & cities['City name'].apply(lambda name: name.startswith('San'))
cities['Is san francisco'] = (cities['City name'] == 'San Francisco')
print(cities)

print(cities.reindex([2, 0, 1]))
