# TBEN

Repository for presentation/code for the [The Black Economists Network](https://www.tben.co.uk/) event
[Introduction to R](https://www.tben.co.uk/events/boe-introtor) 
run by the [Bank of England](https://www.bankofengland.co.uk)'s [Centre for Central Banking Studies](https://www.bankofengland.co.uk/ccbs) on 24 April 2023.

The repository includes the presentation, "R (and RStudio) for Econometricians.pdf" and some code examples.

The two examples are: 

- The first is wrangling the McCracken and Ng FRED monthly database of 127 series 
(MultipleMultiple2.r), then doing ADF tests on each and fitting AR(2) models.
- The second is an IV estimator solution to the biased estimates obtained using OLS when there is a collider 
present (CollideA.r) and draws DAGs of the problem.

An earlier [recording from 17 May 2022](https://vimeo.com/714018017/1879a82119) for a different event has more 
detail on both of these examples, which you may find useful. The [FRED data example begins at 1h:14m:44s](https://vimeo.com/714018017/1879a82119?ts=4480000) and the [IV/DAG example begins at 0h:9m:25s](https://vimeo.com/714018017/1879a82119?ts=565000) . Depending on how you access GitHub 
this may play at exactly the right point, or you may have to find the right timed entry point. On an iPhone it doesn't!
