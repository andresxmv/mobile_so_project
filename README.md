# Projecto sobre Mobile SO

Los datos utilizados son los siguientes:

| Variable                                    | Frequency |    Source   |
|---------------------------------------------|:---------:|:-----------:|
| GDP                                         | Quarterly |     IMF     |
| Population                                  |   Annual  |  World Bank |
| Mobile cellular subscriptions               |  Annual   |  World Bank |
| Market Share (Andriod and iOS)              |  Monthly  | StatCounter |
| R\&D Expenses (Apple and Alphabet)          | Quarterly | Macrotrends |
| Assets and Liabilities (Apple and Alphabet) | Quarterly | Macrotrends |


El panel de datos se encuentra en la carpeta "output", el nombre del archivo es "panel_data.csv". Las columnas
que contiene el .csv son las siguientes:
- year: Fecha en trimestre
- country: Identificador del país
- gdp: GDP por país
- gdp_growth: Tasa de crecimiento de la variable gdp por país
- recession_gdp: Dummy que indica las recesiones basado en la variable GDP por país
- Andriod: Market share de Andriod por país
- iOS: Market share de iOs por país
- diff_so: Diferencia entre el market share de Andriod e iOS por país
- population: Población por país
- gdp_percapita: GDP per capita por país
- gdp_percapita_rate: Tasa de crecimiento del GDP per capita por país
- recession: Dummy que indica las recesiones basado en la variable GDP per capita por país
- quarter: Variable númerica del trimestre
- mobile_subscriptions: Tamaño de la industria de telefonos moviles por país
- mobile_subscriptions_int: Tamaño de la industria de telefonos moviles por país interpolada 
- mobile_growth_rate: Tasa de crecimiento de la industria
- MP: Variable definida en el paper
- r&d_apple: Gasto en R&D por la empresa Apple
- assets_apple: Activos Apple
- liabilities_apple: Pasivos Apple
- liquidity_apple: Liquidez Apple
- r&d_google: Gasto en R&D por la empresa Google (Alphabet)
- assets_google: Activos Google (Alphabet)
- liabilities_google: Pasivos Google (Alphabet)
- liquidity_google: Liquidez Google (Alphabet)

