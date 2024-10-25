## knb-lter-cap.46

### dataset publishing: core bird monitoring

_A note about Flying_: The meaning of 'flying' in the birds table is unclear.
There are 1962 records where flying = 1. All of these except for two records
have distance = FT. However, not all records where distance = FT have a distance
= 1 (or any value). Adding confusion, in her metadata, C. Gries has listed that
flying = NULL is true, but then what would be the meaning of flying = 0, and
that would mean that most birds were flying. My impression is that flying was a
precursor to FT. A "flying" option is not on the current datasheet, nor on an
earlier one revised in 2004. Given that the meaning of "flying" is not clear nor
particularly additive, those data are not included in the published data.

### history

- knb-lter-cap.46.24 *2024-10-24*
  + data refresh
  + update the baseurl parameter in the config file
  + update the r chunk structure to the qmd format

- knb-lter-cap.46.23 *2023-11-01*
  + data refresh
  + incorporates more of the yaml approach, here data entities (except the
  spatial vector) and people are documented via yaml configuration files
  + incorporates QUDT units
  + adds for the first time, albeit limited, annotations, mostly in conjunction
  with the move to QUDT units but also annotations are added to some attributes
  (mostly dates)
  + SQL to isolate CORE surveys at the SRBP sites is broken out into a single,
  temporary table rather than duplicating the code for both the surveys and
  observations queries.
  + Omits duplicate surveys and counts identified by J. Haight

- knb-lter-cap.46.22 *2023-02-12*
  + data refresh
  + workflow modified to adopt `capeml` `update_attributes` function
  + Rmd to qmd
  + update workflow to use local data file for gioseml people functions (had the
  + side effect of for the first time including Paige's middle initial)
  + removed "unidentified" from common names to improve taxonomic name service
  resolution
  + slight edit to abstract formatting (header levels)

- knb-lter-cap.46.21 *2022-11-06*
  + data refresh
  + workflow modified to adopt capeml data_objects.yaml approach for building
  tables.
  + spatial representation of bird_survey_locations made explicitly distinct
  from tabular version as bird_survey_locations_spatial

- knb-lter-cap.46.20 *2022-05-12*
  + data refresh

- knb-lter-cap.46.19 *2021-01-21*
  + This version reflects the first to be based on the new database migrated
  from MySQL::lter34birds to postgresql::core_birds. Data quality and structure
  were greatly improved as part of the migration, which translated into reduced
  and more efficient code at this publishing step.
  + unfortunately, this version does not include new data as, even at this time,
  spring 2019 are still the most recently QC'd data
  + core_birds that had reflected both survey details and bird observations was
  split into separate bird_observations and bird_surveys data tables
  + additional_bird_observations was merged as a single field into bird_surveys
  + full location history provided
    - the only practical way to do this is in tabular form so location data are
    now included as a tabular resource with latitudes and longitudes
    - because any given site can have multiple locations, there was not a
    practical way to present the locations as a spatial resource without somehow
    incorporating begin and end dates so these are now presented only in tabular
    form
    - also because of the aforementioned, only a single bounding box for all
    sites is included for the geographic coverage (rather than a point for each
    site)
  + data limited to most recently QC'd set: 2019-05-03

- knb-lter-cap.46.18 *2020-10-02*
  + data refresh
  + Addresses a problem identified by S. Wittlinger where SRBP surveys were
  omitted. This stems from a related issue with the SRBP birds following a
  change in how the core SRBP site is identified. The solution requires a rather
  details subquery to ensure that only data from SRBP core surveys (i.e., not
  SRBP surveys) are pulled. This logic is applied to all the data products in
  this dataset: core_birds, additional_bird_species, and core_bird_locations.
  + An more verbose version of the logic to query the SRBP surveys to facilitate
  error checking and confirmation is included in this repository as
  `SRBP_core_site_surveys.R`.
  + Include S. Lerman middle initial
  + data limited to most recently QC'd set: 2019-05-03

- knb-lter-cap.46.17 *2020-08-27*
  + data refresh
  + reflects the first publication of the other notable bird species data. Of
  some concern was whether to include the survey start and end times as these
  sightings are not actually part of the survey. Opted to include the times for
  lack of a better approach to indicate the approximate time and connection to
  other survey details, but consider other options in future updates.

-knb-lter-cap.46.16 *2019-04-04*
  + *abstract & methods formatting*: The abstract & methods are unchanged
  between knb-lter-cap.46.15 and .16, so I simply copied the xml from version 15
  into the output for version 16 rather than going through and fixing all of the
  markdown list issues that break the eml.  Hopefully, EML 2.2 will be released
  by the time we visit version 17 such that we can address the markdown properly
  and without having to format by hand after construction.
  + *taxonomicCoverage*: New approach is to use taxonomyCleanr to build the
  taxonomicCoverage. Note that at the time of this writing (and building
  knb-lter-cap.46.16), taxonomyCleanr had not been ported to rOpenSci EML v2, so
  I used a modified version. *Note* that the `taxa_map.csv` built with the
  `create_taxa_map()` function and resolving taxonomic IDs (i.e.,
  `resolve_comm_taxa()`) only needs to be run once per version/session -- the
  taxonomicCoverage can be built as many times as needed with
  `resolve_comm_taxa()` once the `taxa_map.csv` has been generated and the
  taxonomic IDs resolved.
  + *empty missing values*: The problem of NAs as missing values and the empty
  missingValue code that rOpenSci EML v2 produces has not been resolved at this
  time of this writing and constructing knb-lter-cap.46.16 (see
  https://github.com/CAPLTER/capeml/issues/4 for more details). In the meantime,
  using a vim script to remove empty `missingValue` nodes from the eml.
  + *end of line errors*: Carriage returns are not being interpreted properly.
  This is not an R problem as this is an issue even when the data are pulled
  straight from MySQL independently of R. I updated the lter34birds database to
  remove carriage returns from the offending fields (surveys.notes,
  surveys.human_activity_notes, birds.notes), which addressed the problem.
  However, it is possible that carriage returns could be used in future entries
  so be sure to check for this in the final output (using vim to search for `\r`
  is probably easiest).