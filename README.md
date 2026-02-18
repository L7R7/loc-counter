# loc-counter

Count the lines of code for all projects in a GitLab group and visualize the progression over time on an HTML page.

## Configuration

The app expects the following environment variables to be set:

* `GITLAB_API_TOKEN`: API-Token to authenticate against the GitLab API
* `CI_API_V4_URL`: URL to the API base endpoint of your GitLab instance (this variable will be set automatically in CI jobs)
* `GITLAB_GROUP_ID`: ID of the group for which the data should be collected
* `DATABASE_NAME`: file name of the sqlite database. If that file doesn't exist, it will be created.

The following environment variables are optional:

* `USER_AGENT`: User-Agent to set in requests against the GitLab API. Default value is "loc-counter"
* `DEFAULT_LANGUAGES`: comma-separated list of language names that should be shown on the HTML page by default. You can always toggle all languages, this only controls the initial state. Defaults to an empty list
* `KEEP_TMP_DIR`: This is mainly for debugging. If set to `true`, the tmp dir in which the repositories are cloned, won't be deleted after the data collection is finished.

### Provide a list of markers with relevant points in time

You can provide a `markers.csv` file that contains relevant timestamps along with a short text.
These will be shown on the chart with a vertical line.
The expected format is:

```csv
2025-08-22;this is an example marker
2025-09-03;another-marker
```

## Working with the result page

The resulting HTML page can handle the following query parameters:

* `languages`: comma-separated list of languages to show by default. Takes precedence over the `DEFAULT_LANGUAGES` environment variable.

## Usage

The app is intended to be called regularly.
It will collect all the data for all repositories once (with the state of the repositories at the time when it runs), and add it to the database file.
If the database file does not exist, it will create a new one. If it does exist, it will add the data to it.
That means that the caller will have to deal with persisting the database file and providing it for each run.
The data will be recorded for the current day, if there are database entries for the current day they will be overridden.

A proven way to use this app is inside a GitLab CI job that gets triggered regularly (at most once per day, as runs on the same day will override previous runs) and uses job artifacts to handle the persist/restore the database file.
It's also very straightforward to deploy the resulting HTML page to GitLab pages.

