# GDP per capita - Data package

This data package contains the data that powers the chart ["GDP per capita"](https://ourworldindata.org/grapher/gdp-per-capita-worldbank?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website.

### Active Filters

A filtered subset of the full data was downloaded. The following filters were applied:

## CSV structure

Each row is an observation for an entity (usually a country or region) at a timepoint.

- "Entity" — the name of the entity, e.g. "United States".
- "Code" — our internal entity code. For most countries this is the [ISO alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code, e.g. "USA"; historical and other non-standard entities get a custom code.
- "Year" or "Day" — the timepoint. Annual data has a "Year" column holding an integer year; otherwise a "Day" column holds a date string in the form "YYYY-MM-DD".
- Every remaining column is a data column, each one a time series. Downloaded with the "full data" option each corresponds to one time series below; with "only selected data visible in the chart" they are transformed depending on the chart type, so the correspondence may be less direct.


## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc.

## How we process data at Our World in Data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stitch together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

Preparing this data involves several processing steps. Depending on the data, this can include standardizing country names and world region definitions, converting units, calculating derived indicators such as per capita measures, as well as adding or adapting metadata such as the name or the description given to an indicator.
[Read about our data pipeline](https://docs.owid.io/projects/etl/).

## Detailed information about each time series


### GDP per capita – In constant international-$ – World Bank
Average economic output per person in a country or region per year. This data is adjusted for inflation and differences in living costs between countries.
Last updated: July 27, 2026  
Next expected update: January 2027  
Date range: 1990–2025  
Unit: international-$ in 2021 prices  
Source: Eurostat, OECD, IMF, and World Bank (2026) – with minor processing by Our World in Data  

#### How to cite this data

Eurostat, OECD, IMF, and World Bank (2026) – with minor processing by Our World in Data

#### What you should know about this data
- GDP per capita is a comprehensive measure of people's average income. It helps compare income levels across countries and track how they change over time. It is especially useful for understanding trends in economic growth and living standards.
- GDP per capita is calculated as the value of all final goods and services produced each year in a country (the gross domestic product), divided by the population. It represents the average economic output per person.
- This indicator shows the large inequality between people in different countries. In the poorest countries, average incomes are below $1,000 per year; in rich countries, they are more than 50 times higher.
- This data is expressed in constant international dollars to adjust for inflation and differences in living costs between countries. Read more in our article, [What are international dollars?](https://ourworldindata.org/international-dollars)
- This data comes from the World Bank and starts in 1990. For estimates going back several centuries, explore our chart of GDP per capita from the [Maddison Project Database](https://ourworldindata.org/grapher/gdp-per-capita-maddison-project-database).

#### How this data is described by its producers
This indicator provides values for gross domestic product (GDP) per person expressed in constant international dollars, converted by purchasing power parities (PPPs). PPPs account for the different price levels across countries and thus PPP-based comparisons of economic output are more appropriate for comparing the output of economies and the average material well-being of their inhabitants than exchange-rate based comparisons.

Gross domestic product is the total income earned through the production of goods and services in an economic territory during an accounting period. It can be measured in three different ways: using either the expenditure approach, the income approach, or the production approach. The core indicator has been divided by the general population to achieve a per capita estimate. This indicator is expressed in constant prices, meaning the series has been adjusted to account for price changes over time. The reference year for this adjustment is 2021. The PPP conversion factor is a currency conversion factor and a spatial price deflator. PPPs convert different currencies to a common currency and, in the process of conversion, equalize their purchasing power by eliminating the differences in price levels between countries, thereby allowing volume or output comparisons of GDP and its expenditure components.

### Aggregation method:
Weighted average

### Statistical concept and methodology:
Methodology: The International Comparison Program (ICP) estimates PPPs for the world’s countries. The ICP is conducted as a global partnership of countries, multilateral agencies, and academia. The recent 2021 ICP comparison covered 176 countries, including 49 Eurostat-OECD countries. For countries that have not participated in ICP comparisons, the PPP are imputed based on a regression model. ICP estimated PPPs cover years from 2011 to 2021. WDI extrapolates 2011 PPPs for years earlier years, and 2021 PPPs for later years. For the member countries of Eurostat-OECD PPP Programme, PPP conversion factors are periodically updated based on the organizations’ databases.

National accounts are compiled in accordance with international standards: System of National Accounts, 2008 or 1993 versions. Specific information on how countries compile their national accounts can be found on the IMF website: https://dsbb.imf.org/ Linked series have been smoothed to remove breaks resulting from changes in base years, data sources or compilation methods. The linking is performed using historical nominal growth rates from archived WDI databases.

Statistical concept(s): PPPs are primarily used to convert the national accounts data of economies, such as GDP and its expenditure components, into a common currency. In the process of conversion, they control for differences in the price levels of economies, and thus equalize purchasing power. PPP-based comparisons of economic output differ from market exchange rate-based comparisons as the latter do not distinguish between the relative price levels of different items in economies. Overall price levels are normally higher in higher-income economies than they are in lower-income economies (Balassa-Samuelson effect), mostly because of the large differences in price levels for non-traded products. If no account is taken of the larger price level differences for non-traded products when converting GDP to a common currency, the size of higher-income economies with high price levels will be overstated and the size of lower-income economies with low price levels will be understated. No distinction is made between traded products and non-traded products when market exchange rates are used to convert GDP to a common currency: the rate is the same for all products. PPP-converted GDP does not have this bias because PPPs account for the different price levels of traded products and non-traded products. Thus, PPPs are more appropriate for comparing the output of economies and the average material well-being of their inhabitants and are also less impacted by the potential volatility of market exchange rates. PPPs are calculated by the International Comparison Program (ICP) based on the prices of goods and services within an economy and national accounts expenditures.

The conceptual elements of the SNA (System of National Accounts) measure what takes place in the economy, between which agents, and for what purpose. At the heart of the SNA is the production of goods and services. These may be used for consumption in the period to which the accounts relate or may be accumulated for use in a later period. In simple terms, the amount of value added generated by production represents GDP. The income corresponding to GDP is distributed to the various agents or groups of agents as income and it is the process of distributing and redistributing income that allows one agent to consume the goods and services produced by another agent or to acquire goods and services for later consumption. The way in which the SNA captures this pattern of economic flows is to identify the activities concerned by recognizing the institutional units in the economy and by specifying the structure of accounts capturing the transactions relevant to one stage or another of the process by which goods and services are produced and ultimately consumed.

### Development relevance:
PPPs are used to convert national accounts data from different countries, such as GDP and its expenditure components, into a common currency, while also eliminating the effect of price level differences between countries. PPPs are also used to derive price level indexes (PLIs), the ratio of a country’s PPP to its market exchange rate, to directly compare price levels across countries.
The PPP-based expenditures to which they give rise are primarily used to make spatial comparisons of volume and per capita consumption or levels of GDP and its expenditure components across countries. PPP-based indicators are used for national, regional, and global policy making and analysis across the socioeconomic spectrum from poverty and inequality, to health and education, to energy and climate, through to economic growth, labor, productivity, trade, competitiveness, and infrastructure. A number of Sustainable Development Goals use PPP-based indicators to measure development progress.

This indicator is related to the national accounts, which are critical for understanding and managing a country's economy. They provide a framework for the analysis of economic performance. National accounts are the basis for estimating the Gross Domestic Product (GDP) and Gross National Income (GNI), which are the most widely used indicator of economic performance. They are essential for government policymakers, providing the data needed to design and assess fiscal and monetary policies; and are also used by businesses and investors to assess the economic climate and make investment decisions. NAS enable comparison between economies, which is crucial for international trade, investment decisions, and economic competitiveness. More specifically, this indicator is related to national accounts aggregates. Gross Domestic Product (GDP), Gross National Income (GNI), and other aggregates provide a snapshot of the size and health of an economy by measuring the total economic activity within a country. They can thus be used by policymakers to design and implement economic policies, as they reflect the overall economic performance and can indicate the need for intervention in certain areas. Aggregates also allow for comparisons between different economies, which can be useful for trade negotiations, investment decisions, and economic benchmarking. By examining aggregates over time, economists and analysts can identify trends, cycles, and potential areas of concern within an economy, and investors can use national accounts aggregates to assess the potential risks and returns of investing in a particular country. Overall, national accounts aggregates are fundamental tools for economic analysis, policy formulation, and decision-making at both the national and international levels.


### World region according to OWID
Regions defined by Our World in Data, which are used in OWID charts and maps.
Last updated: January 1, 2023  
Date range: 2023–2023  
Source: Our World in Data  

#### How to cite this data

Our World in Data


## Sources

These are the sources behind the data in this package. Each time series above names the ones it draws on in its citation.

### Eurostat, OECD, IMF, and World Bank – World Development Indicators

The World Development Indicators (WDI) database, published by the World Bank, is a comprehensive collection of global development data, providing key economic, social, and environmental statistics. It includes over 1,500 indicators covering more than 200 countries and territories, with data spanning several decades. WDI serves as a vital resource for policymakers, researchers, businesses, and analysts seeking to understand global trends and make data-driven decisions. The database covers a wide range of topics, including economic growth, education, health, poverty, trade, energy, infrastructure, governance, and environmental sustainability. The indicators are sourced from reputable national and international agencies, ensuring high-quality, consistent, and comparable data. Users can access the database through interactive online tools, API services, and downloadable datasets, facilitating detailed analysis and visualization. WDI is also used for tracking progress on the Sustainable Development Goals (SDGs) and other global development initiatives. By providing accessible and reliable statistics, it helps to inform policy discussions and strategies globally. Whether for academic research, policy planning, or economic analysis, the World Development Indicators database is an essential tool for understanding and addressing global development challenges.

Producer: Eurostat, OECD, IMF, and World Bank  
Published: 2026-07-15  
Retrieved on: 2026-07-27  
Retrieved from: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD  
Direct download: https://databankfiles.worldbank.org/public/ddpext_download/WDI_CSV.zip  
License: CC BY 4.0 (https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators)  

Citation: International Comparison Program (ICP), World Bank (WB), uri: https://www.worldbank.org/en/programs/icp/data, note: This information is for ICP’s PPPs utilized in WDI, publisher: International Comparison Program (ICP), date accessed: May 30, 2024, date published: May 30, 2024;
The Eurostat PPP Programme, Eurostat (ESTAT), uri: https://ec.europa.eu/eurostat/databrowser/explore/all/all_themes, publisher: Eurostat;
The OECD PPP Programme, Organisation for Economic Co-operation and Development (OECD), uri: https://data-explorer.oecd.org/, publisher: OECD;
Staff estimates, World Bank (WB);
National Accounts data files, Organisation for Economic Co-operation and Development (OECD);
World Economic Outlook database, International Monetary Fund (IMF). Indicator NY.GDP.PCAP.PP.KD (https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD). World Development Indicators – World Bank (2026). Accessed on 2026-07-27.

### Our World in Data – Regions

Producer: Our World in Data  

    