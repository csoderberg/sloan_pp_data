/* query to determine the number of 'submitted' and 'started by never submitted' preprint in 2020 by month and provider */

/*set up time windows and 'finished' state categorizations */
WITH pp_finished AS (SELECT id, 
						provider_id, created, 
						machine_state, 
						CASE WHEN machine_state = 'initial' THEN 'not_finished' ELSE 'finished' END AS pp_submit_finished,
						CASE WHEN osf_preprint.created < '2020-06-04 14:42:50.148795+00:00' THEN 'pre_exp' ELSE 'during_exp' END AS timeframe
					FROM osf_preprint
					WHERE osf_preprint.created >= '2020-04-30 20:37:53.449468+00:00' AND osf_preprint.created <= '2020-07-09 14:42:50.148795+00:00' AND 
							provider_id != 7)

/* count up preprints by finished state and provider during/before experiment */
SELECT COUNT(pp_finished.id) num_pps, 
		timeframe, 
		osf_abstractprovider._id AS provider, 
		pp_submit_finished
	FROM pp_finished
	LEFT JOIN osf_abstractprovider
	ON pp_finished.provider_id = osf_abstractprovider.id
	GROUP BY pp_submit_finished, osf_abstractprovider._id, timeframe



/* query to get number of started, but not completed preprints, that got to each stage of the upload process */

/* set up categorization of preprints into time windows */
WITH pp_timing AS (SELECT id,
						created,
						CASE WHEN osf_preprint.created < '2020-06-04 14:42:50.148795+00:00' THEN 'pre_exp' ELSE 'during_exp' END AS timeframe
					FROM osf_preprint
					WHERE osf_preprint.created >= '2020-04-30 20:37:53.449468+00:00' AND osf_preprint.created <= '2020-07-09 14:42:50.148795+00:00' AND 
							provider_id != 7 AND machine_state = 'initial')

/* count up the number of preprints with each action type before/during the experiment' */
SELECT COUNT(pp_timing.id) AS num_pps, action, timeframe
	FROM pp_timing
	LEFT JOIN (SELECT preprint_id, action, MIN(created)
				FROM osf_preprintlog
				WHERE created >= '2020-04-30 20:37:53.449468+00:00'
				GROUP BY preprint_id, action) AS pp_actions
	ON pp_timing.id = pp_actions.preprint_id
	GROUP BY action, timeframe


/* count of the has_coi status of non-finished preprints*/
SELECT COUNT(id) as num_pp, has_coi
	FROM osf_preprint
	WHERE osf_preprint.created >= '2020-06-04 14:42:50.148795+00:00' AND osf_preprint.created <= '2020-06-25 14:42:50.148795+00:00' AND 
			provider_id != 7 AND machine_state = 'initial'
	GROUP BY has_data_links




/* query to pull preprints published during the timewindow that have data statements (some have been publisehd during the timeframe but not have data statements if they entered pre-mod before the time window) */
SELECT osf_guid._id AS guid, has_coi, osf_abstractprovider._id AS pp_provider, osf_preprint.id AS pp_num, machine_state, date_withdrawn,
		date_published, log_date, action AS log_action, params ->> 'value' AS log_coi_value, has_data_links, data_links, why_no_data, article_doi
	FROM osf_preprint
	LEFT JOIN osf_guid
	ON osf_preprint.id = osf_guid.object_id AND content_type_id = 47
	LEFT JOIN osf_abstractprovider
	ON osf_preprint.provider_id = osf_abstractprovider.id
	LEFT JOIN (SELECT created AS log_date, action, params, preprint_id
					FROM osf_preprintlog
					WHERE action = 'has_data_links_updated' OR action = 'has_coi_updated') AS statement_logs
	ON osf_preprint.id = statement_logs.preprint_id
	WHERE osf_preprint.created >= '2020-06-04 14:42:50.148795+00:00' AND osf_preprint.created <= '2020-06-25 14:42:50.148795+00:00' AND 
		provider_id != 7 AND (spam_status IS NULL OR spam_status != 2) AND has_data_links IS NOT NULL AND ever_public IS TRUE AND is_published IS TRUE