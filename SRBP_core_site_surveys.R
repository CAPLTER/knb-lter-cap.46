# More versbose form of logic to query only core surveys at SRBP sites. This
# separate script is included in this repository to facilitate easier
# confirmation and error-checking that queried surveys at SRBP sites are in
# fact core surveys.

srbp_core_surveys <- dbGetQuery(mysql_prod, "
    SELECT
      surveys.survey_id,
      sites.site_code,
      sites.sample AS location_type,
      surveys.survey_date,
      surveys.time_start,
      surveys.time_end,
      surveys.observer
    FROM lter34birds.surveys
    JOIN lter34birds.sites ON (surveys.site_id = sites.site_id)
    JOIN (
      SELECT
        subquery.site_id,
        subquery.site_code,
        subquery.reach,
        GREATEST(
          IFNULL(begin_date, '2000-01-01'),
          IFNULL(begin, '2000-01-01')
          ) AS begin_date,
        LEAST(
          IFNULL(end_date, DATE(NOW())),
          IFNULL(end, DATE(NOW()))
          ) AS end_date
      FROM(
        SELECT
          srcs.site_id,
          sites.site_code,
          srcs.reach,
          srcs.begin_date,
          srcs.end_date,
          CASE
            WHEN srcs.end_date IS NULL THEN '2000-01-01'
          END AS begin,
          CASE
            WHEN srcs.end_date IS NULL THEN DATE(NOW())
          END AS end
        FROM lter34birds.salt_river_core_sites srcs
        JOIN lter34birds.sites ON (sites.site_id = srcs.site_id)
      ) AS subquery
      ) AS core_site_date ON (
        surveys.site_id = core_site_date.site_id AND
        surveys.survey_date BETWEEN core_site_date.begin_date AND core_site_date.end_date
      )
      WHERE
        (
          MONTH(surveys.survey_date) IN (1, 2, 3, 4) OR
            (MONTH(surveys.survey_date) IN (5) AND DAY(surveys.survey_date) <= 20)
          ) OR
        MONTH(surveys.survey_date) IN (12) AND DAY(surveys.survey_date) >= 15
      ;")
