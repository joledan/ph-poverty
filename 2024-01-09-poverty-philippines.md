---
layout: post
title: Poverty in the Philippines
thumbnail: ""
---

**[code](https://github.com/joledan/ph-poverty) and tools:** R, ggplot2

<!--more-->

<!--![fig1]({{site.url}}/assets/images/wiki-nc/wiki-nc-fig1.png)-->

[intro and comparison to SEA neighbours]
[brief history of poverty reduction efforts]
[current efforts in poverty reduction]

<!-- https://kidb.adb.org/explore?filter[year]=2000%2C2001%2C2002%2C2003%2C2004%2C2005%2C2006%2C2007%2C2008%2C2009%2C2010%2C2011%2C2012%2C2013%2C2014%2C2015%2C2016%2C2017%2C2018%2C2019%2C2020%2C2021%2C2022%2C2023&filter[indicator_id]=3010010&filter[economy_code]=BRU%2CCAM%2CINO%2CLAO%2CMAL%2CMYA%2CPHI%2CSIN%2CTHA%2CTIM%2CVIE&showRegions=false&grouping=indicators -->

The Philippines, a populous island nation in Southeast Asia, has ambitious goals for poverty reduction. By 2028 President Ferdinand Marcos Jr aims to [decrease](https://asia.nikkei.com/Economy/Philippine-poverty-rate-drops-to-22.4-still-far-from-Marcos-target) the country's poverty rate to 9% as his presidential term ends. In line with the UN Sustainable Development Goals, the country aims to eradicate extreme poverty by 2030. A susceptibility to natural disasters, coupled with [high food inflation](neda.gov.ph/ph-records-lowest-inflation-rate-in-2023-govt-to-continue-measures-to-protect-filipino-purchasing-power-neda/), and a history of conflict has stalled progress in the Philippines. 

From 2012 to 2021 (the most recent year of data) the Philippines' national poverty rate dropped from 25.2% to 18.1% (see chart 1), still above the country's [target](https://psa.gov.ph/statistics/statdev/press-release) between 15.5-17.5%. This was accomplished through various social welfare policies such as [cash transfer programs](https://www.officialgazette.gov.ph/programs/conditional-cash-transfer/), community driven development, and improvements to livelihood and [employment opportunities]((https://www.lumina.com.ph/news-and-blogs/blogs/social-welfare-programs-in-the-philippines/)) for the most vulnerable populations. 

<!-- average poverty rate by region -->
The Philippines is split into three major island groups from north to south: Luzon, Visayas, and Mindanao. The National Capital Region (NCR) contains the capital Manila and 16 other cities and municipalities. The national poverty rate is defined as the share of poor Filipinos whose income per person is insufficient to meet their basic needs. Poverty rate by major island group follows the reverse pattern from top to bottom: Mindanao records the highest poverty rate, followed by Visayas, Luzon (excluding the NCR), and the NCR recording the lowest values (see chart 2). The overall poverty rate cuts through the middle.

From 2015 to 2018, all major island groups recorded decreases in poverty rates. The impact of the 2020 COVID-19 pandemic resulted in increased poverty rates in the country as a whole and in Luzon, Visayas, and the NCR, from 2018 to 2021. Mindanao is the only island group that saw a constant decrease in poverty over the three years of data. However, it still records the highest poverty rate among the major island groups in the country and the order has not changed over time. 

<!-- describe mindanao if needed? https://www.economist.com/asia/2017/11/25/the-philippines-has-the-most-persistent-poverty-in-south-east-asia
or why mindanao has decreased over time  -->

<!-- urban/rural poverty -->
Most regions in Luzon and the NCR, the Philippines' economic centre, record lower urban and rural poverty rates compared to other regions elsewhere in the country (see chart 2). 

The urban-rural poverty divide is stark: in 15 out of 17 regions, rural poverty rates are higher than urban rates. All regions in Visayas and Mindanao, with predominantly agriculture and fishing based economies, record urban poverty rates above the national value. 



Urban poverty is still 
Geographic differences in urban poverty . 

Even if there are greater opportunities in cities and towns, all urban poverty rates in regions in Visayas and Mindanao exceed the national value.  These values suggest that more work must be done to shrink the urban-rural poverty gap. 


<!-- need to conclude on poverty programs and where to go -->

[next plot to make?]
[why is Mindanao's poverty rate high relative to the other regions?]
[is it urban/rural differences?]




<!-- definition fo poverty incidence https://psa.gov.ph/statistics/poverty/node/162559 -->
<!-- increase in poverty from 2018-2021 https://www.reuters.com/world/asia-pacific/pandemic-pushed-millions-more-into-poverty-philippines-govt-2022-08-15/ -->


[This Wikipedia article](https://en.wikipedia.org/wiki/Geographical_renaming) contains a (incomplete) list of significant country name changes from the 1500s to present day. Changing names is not an easy task as there are a lot of factors to consider. Does currency need to be reprinted? What about names of established institutions? Rebranding can be costly, but is often done out of necessity. 

-----

## Reflections

There are a lot of packages to facilitate scraping in R. This also gave me a chance to brush up on my HTML/XPath skills that I picked up during my studies. Scraping a simple list from Wikipedia was not too challenging; I think the most difficult part of this task was re-structuring and cleaning the text data into a neat and usable format. 