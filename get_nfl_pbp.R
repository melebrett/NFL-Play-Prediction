library(tidyverse)
library(nflfastR)

##### Load Data
all_pbp <- load_pbp(2012:2022)
all_stats <- load_player_stats(2012:2022)

nrow(all_pbp %>% distinct(game_id))
# colnames(all_pbp)
# https://nflreadr.nflverse.com/articles/dictionary_pbp.html

##### Filter and get necessary columns #####
# only regular season games
all_pbp <- all_pbp %>% 
    filter(season_type == 'REG' & (play + field_goal_attempt + punt_attempt ) > 0 )

# select necessary columns and drop kickoffs, extra points, no play
all_pbp <- all_pbp %>% 
    mutate(
        drive_start = ifelse(drive_play_id_started == play_id,1,0),
        pass_attempt = pass,
        rush_attempt = ifelse(rush_attempt == 1 & pass == 0, 0, rush_attempt),
    ) %>%
    dplyr::select(
        season, game_id, season_type, home_team, away_team, spread_line, total_line, game_date, week, play_id, desc, play_type, two_point_attempt, drive, series, drive_play_count, drive_time_of_possession,
        posteam, posteam_type, defteam, yardline_100, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, qtr, game_half,
        quarter_end, drive_start, down, goal_to_go, ydstogo, qb_dropback, qb_spike, qb_scramble, no_huddle, shotgun, run_location, home_timeouts_remaining, away_timeouts_remaining, score_differential,
        total_home_rush_epa, total_home_pass_epa, total_away_rush_epa, total_away_pass_epa, total_home_rush_wpa, total_away_rush_wpa, total_home_pass_wpa, total_away_pass_wpa,
        passer_player_id, qb_epa, roof, surface, temp, wind, rush_attempt, pass_attempt, field_goal_attempt, punt_attempt, penalty, penalty_team, penalty_yards, timeout, timeout_team,
        qb_hit, pass_length, pass_location, air_yards, yards_after_catch, air_epa, yac_epa, first_down, yards_gained, epa, wpa, wp, ep
) %>% 
    filter(!play_type %in% c('kickoff', 'no_play', 'extra_point', 'qb_kneel') & two_point_attempt == 0) %>% 
    drop_na(play_type)

# all_pbp %>% filter(posteam == 'ARI' & game_date == '2020-10-11') %>% filter(is.na(down)) %>% head(2) %>% glimpse()

##### Cumulative Season Team Stats #####
# pos team cumulative play rates, offense epa/wpa up to that point in season
pt_play_rates_cumulative <- all_pbp %>% 
    group_by(posteam, season, game_date) %>%
    arrange(drive, play_id) %>%
    mutate(play_count = row_number()) %>%
    summarise(
        first_downs = sum(ifelse(down==1, 1, 0)),
        second_downs = sum(ifelse(down==2, 1, 0)),
        third_downs = sum(ifelse(down==3, 1, 0)),
        fourth_downs = sum(ifelse(down==4, 1, 0)),
        neutral_wp = sum(ifelse(wp > 0.1 & wp < 0.9, 1,0)),
        goal_to_go = sum(goal_to_go),
        scripted = sum(ifelse(play_count <= 15, 1, 0)),
        rush_attempts = sum(rush_attempt),
        pass_attempts = sum(pass_attempt),
        fd_pass = sum(ifelse(down==1, pass_attempt, 0)),
        sd_pass = sum(ifelse(down==2, pass_attempt, 0)),
        td_pass = sum(ifelse(down==3,pass_attempt, 0)),
        neutral_pass = sum(ifelse(wp > 0.1 & wp < 0.9,pass_attempt, 0)),
        gtg_pass = sum(ifelse(goal_to_go ==1, pass_attempt, 0)),
        scripted_pass = sum(ifelse(play_count <= 15, pass_attempt, 0)),
        run_left = sum(ifelse(rush_attempt == 1 & coalesce(run_location,'middle') == 'left', 1, 0)),
        run_right = sum(ifelse(rush_attempt == 1 & coalesce(run_location, 'middle') == 'right', 1, 0)),
        rush_epa = sum(epa * rush_attempt),
        pass_epa = sum(epa * pass_attempt),
        rush_wpa = sum(wpa * rush_attempt),
        pass_wpa = sum(wpa * pass_attempt),
    ) %>%
    group_by(posteam, season) %>%
    arrange(game_date) %>%
    mutate(
        pt_fd_pass_pct = cumsum(fd_pass)/cumsum(first_downs),
        pt_sd_pass_pct = cumsum(sd_pass)/cumsum(second_downs),
        pt_td_pass_pct = cumsum(td_pass)/cumsum(third_downs),
        pt_gtg_pass_pct = cumsum(gtg_pass)/cumsum(goal_to_go),
        pt_neutral_pass_pct = cumsum(neutral_pass)/cumsum(neutral_wp),
        pt_scripted_pass_pct = cumsum(scripted_pass)/cumsum(scripted),
        pt_pass_pct = cumsum(pass_attempts)/cumsum((pass_attempts + rush_attempts)),
        pt_run_left_pct = cumsum(run_left)/cumsum(rush_attempts),
        pt_run_right_pct = cumsum(run_right)/cumsum(rush_attempts),
        pt_rush_epa_play = cumsum(rush_epa)/cumsum(rush_attempts),
        pt_pass_epa_play = cumsum(pass_epa)/cumsum(pass_attempts),
        pt_rush_wpa_play = cumsum(rush_wpa)/cumsum(rush_attempts),
        pt_pass_wpa_play = cumsum(pass_wpa)/cumsum(pass_attempts)
    ) %>% 
    group_by(posteam) %>%
    arrange(season, game_date) %>%
    mutate(game_date = lead(game_date)) %>%
    ungroup() %>%
    group_by(season) %>% 
    mutate_if(is.numeric,~coalesce(.,mean(.,na.rm = T))) %>%
    ungroup()

# colSums(is.na(test))

# defense team cumulative play rates, offense epa/wpa up to that point in season
dt_play_rates_cumulative <- all_pbp %>% 
    group_by(defteam, season, game_date) %>%
    arrange(drive, play_id) %>%
    mutate(play_count = row_number()) %>%
    summarise(
        first_downs = sum(ifelse(down==1, 1, 0)),
        second_downs = sum(ifelse(down==2, 1, 0)),
        third_downs = sum(ifelse(down==3, 1, 0)),
        fourth_downs = sum(ifelse(down==4, 1, 0)),
        neutral_wp = sum(ifelse(wp > 0.1 & wp < 0.9, 1,0)),
        goal_to_go = sum(goal_to_go),
        scripted = sum(ifelse(play_count <= 15, 1, 0)),
        rush_attempts = sum(rush_attempt),
        pass_attempts = sum(pass_attempt),
        fd_pass = sum(ifelse(down==1, pass_attempt, 0)),
        sd_pass = sum(ifelse(down==2, pass_attempt, 0)),
        td_pass = sum(ifelse(down==3,pass_attempt, 0)),
        neutral_pass = sum(ifelse(wp > 0.1 & wp < 0.9,pass_attempt, 0)),
        gtg_pass = sum(ifelse(goal_to_go ==1, pass_attempt, 0)),
        scripted_pass = sum(ifelse(play_count <= 15, pass_attempt, 0)),
        run_left = sum(ifelse(rush_attempt == 1 & coalesce(run_location,'middle') == 'left', 1, 0)),
        run_right = sum(ifelse(rush_attempt == 1 & coalesce(run_location, 'middle') == 'right', 1, 0)),
        rush_epa = sum(epa * rush_attempt),
        pass_epa = sum(epa * pass_attempt),
        rush_wpa = sum(wpa * rush_attempt),
        pass_wpa = sum(wpa * pass_attempt),
    ) %>%
    group_by(defteam, season) %>%
    arrange(game_date) %>%
    mutate(
        dt_fd_pass_pct = cumsum(fd_pass)/cumsum(first_downs),
        dt_sd_pass_pct = cumsum(sd_pass)/cumsum(second_downs),
        dt_td_pass_pct = cumsum(td_pass)/cumsum(third_downs),
        dt_gtg_pass_pct = cumsum(gtg_pass)/cumsum(goal_to_go),
        dt_neutral_pass_pct = cumsum(neutral_pass)/cumsum(neutral_wp),
        dt_scripted_pass_pct = cumsum(scripted_pass)/cumsum(scripted),
        dt_pass_pct = cumsum(pass_attempts)/cumsum((pass_attempts + rush_attempts)),
        dt_run_left_pct = cumsum(run_left)/cumsum(rush_attempts),
        dt_run_right_pct = cumsum(run_right)/cumsum(rush_attempts),
        dt_rush_epa_play = cumsum(rush_epa)/cumsum(rush_attempts),
        dt_pass_epa_play = cumsum(pass_epa)/cumsum(pass_attempts),
        dt_rush_wpa_play = cumsum(rush_wpa)/cumsum(rush_attempts),
        dt_pass_wpa_play = cumsum(pass_wpa)/cumsum(pass_attempts)
    ) %>% 
    ungroup() %>% 
    group_by(defteam) %>%
    arrange(season, game_date) %>%
    mutate(game_date = lead(game_date)) %>%
    ungroup() %>% 
    group_by(season) %>% 
    mutate_if(is.numeric,~coalesce(.,mean(.,na.rm = T))) %>%
    ungroup()

##### Cumulative Season QB stats #####
# QB stats up to that point in season regressed to prior season weighted avg
# cumulative per season
qb_cumulative <- all_pbp %>% 
    drop_na(passer_player_id) %>%
    group_by(passer_player_id, season, game_date) %>%
    arrange(drive, play_id) %>%
    mutate(play_count = row_number()) %>%
    summarise(
        pass_attempts = sum(pass_attempt),
        pass_epa = sum(epa * pass_attempt),
        pass_wpa = sum(wpa * pass_attempt)
    ) %>%
    group_by(passer_player_id, season) %>%
    arrange(game_date) %>%
    mutate(
        qb_pass_attempts = cumsum(pass_attempts),
        qb_pass_epa_play = cumsum(pass_epa)/cumsum(pass_attempts),
        qb_pass_wpa_play = cumsum(pass_wpa)/cumsum(pass_attempts)
    ) %>% 
    mutate_if(is.numeric,~coalesce(.,0)) %>%
    ungroup()

# end of season
qb_season <- qb_cumulative %>% 
    group_by(passer_player_id, season) %>% 
    arrange(game_date) %>% 
    slice_tail(n = 1) %>% 
    dplyr::select(-game_date)

# join end of season stats for last 3 years, use prior season weighted average as prior and regress cumulative stats to mean
qb_cumulative <- qb_cumulative %>% 
    left_join(
        qb_season %>% mutate(season = season + 1) %>% rename_all(~paste(.,"_1", sep = "")),
        by = c("passer_player_id" = "passer_player_id_1", "season" = "season_1")
    ) %>% 
        left_join(
        qb_season %>% mutate(season = season + 2) %>% rename_all(~paste(.,"_2", sep = "")),
        by = c("passer_player_id" = "passer_player_id_2", "season" = "season_2")
    ) %>% 
        left_join(
        qb_season %>% mutate(season = season + 3) %>% rename_all(~paste(.,"_3", sep = "")),
        by = c("passer_player_id" = "passer_player_id_3", "season" = "season_3")
    ) %>% 
    group_by(season) %>%
    mutate(
        rep_epa_play = quantile(qb_pass_epa_play_1, 0.2, na.rm=T),
        rep_wpa_play = quantile(qb_pass_wpa_play_1, 0.2, na.rm = T),
        across(c(starts_with('qb_pass_')), ~ifelse(is.na(.), quantile(.,0.2, na.rm=T), .)),
        rc = quantile(qb_pass_attempts_1, 0.25, na.rm =T),
        prior_pass_epa_play = (0.6*(qb_pass_epa_play_1 * qb_pass_attempts_1) + 0.3*(qb_pass_epa_play_2 * qb_pass_attempts_2) + 0.1*(qb_pass_epa_play_3 * qb_pass_attempts_3) + rep_epa_play * rc) /
            (0.6*qb_pass_attempts_1 + 0.3*qb_pass_attempts_2 + 0.1*qb_pass_attempts_3 + rc),
        prior_pass_wpa_play = (0.6*(qb_pass_wpa_play_1 * qb_pass_attempts_1) + 0.3*(qb_pass_wpa_play_2 * qb_pass_attempts_2) + 0.1*(qb_pass_wpa_play_3 * qb_pass_attempts_3) + rep_wpa_play * rc)/
            (0.6*qb_pass_attempts_1 + 0.3*qb_pass_attempts_2 + 0.1*qb_pass_attempts_3 + rc),
        qb_pass_epa_play_adj = (qb_pass_epa_play * qb_pass_attempts + rc * prior_pass_epa_play)/(rc + qb_pass_attempts),
        qb_pass_wpa_play_adj = (qb_pass_wpa_play * qb_pass_attempts + rc * prior_pass_wpa_play)/(rc + qb_pass_attempts),
    ) %>% 
    ungroup() %>% 
    left_join(
        all_stats %>% distinct(player_id, player_display_name),
        by = c("passer_player_id" = "player_id")
    ) %>%
    dplyr::select(
        passer_player_id, passer_name = player_display_name, season, game_date, qb_pass_attempts, qb_pass_epa_play_adj, qb_pass_wpa_play_adj
    )

qb_cumulative %>% 
    group_by(passer_player_id, season) %>% 
    arrange(desc(game_date)) %>% 
    slice_head(n = 1) %>% 
    arrange(-qb_pass_epa_play_adj) %>%
    head(15)

##### Team Game/Play numbers #####
# posteam game number and away team game number
pt_game_no <- all_pbp %>% 
    distinct(posteam, season, game_date) %>% 
    group_by(posteam, season) %>%
    arrange(game_date) %>%
    mutate(
        pos_team_game_no = row_number()
    )

dt_game_no <- all_pbp %>% 
    distinct(defteam, season, game_date) %>% 
    group_by(defteam, season) %>%
    arrange(game_date) %>%
    mutate(
        def_team_game_no = row_number()
    )

all_pbp <- all_pbp %>%    
    group_by(posteam, game_date) %>% 
    arrange(drive, play_id) %>% 
    mutate(
        pt_play_no = row_number(),
        pt_spread = ifelse(posteam_type == 'away', -1.0*spread_line, spread_line)
    )  %>% 
    ungroup()

# all_pbp %>% filter(posteam == 'ARI' & game_date == '2020-10-11') %>% select(posteam, defteam, posteam_type, spread_line, pt_spread) %>% head(1)

##### Combine #####
all_pbp <- all_pbp %>% 
    left_join(
        pt_play_rates_cumulative %>% dplyr::select(posteam, season, game_date, starts_with('pt_')),
        by = c("posteam", "season", "game_date")
    ) %>% 
    left_join(
        dt_play_rates_cumulative %>% dplyr::select(defteam, season, game_date, starts_with('dt_')),
        by = c("defteam", "season", "game_date")
    )

all_pbp <- all_pbp %>% 
    left_join(pt_game_no) %>%
    left_join(dt_game_no)

# fill the passer and add qb cumulative stats
all_pbp <- all_pbp %>% 
    group_by(posteam, game_date) %>%
    arrange(pt_play_no) %>% 
    fill(passer_player_id, .direction = "downup") %>% 
    ungroup() %>% 
    left_join(
        qb_cumulative,
        by = c("passer_player_id", "game_date", "season")
    )

# fix missing numeric columns
all_pbp <- all_pbp %>% 
    group_by(season) %>%
    mutate(
        across(c(qb_pass_wpa_play_adj, qb_pass_epa_play_adj, air_yards, yards_after_catch, air_epa, yac_epa, qb_epa, epa, wind, penalty_yards, yards_gained), ~coalesce(.,0)),
        across(c(starts_with('pt_'), starts_with('dt')), ~coalesce(.,mean(., na.rm = T)))
    ) %>% 
    ungroup()

# fix missing categorical columns
# roof & temp
# all_pbp %>% distinct(roof)
# all_pbp %>% filter(is.na(roof)) %>% distinct(game_id)
# all_pbp %>% distinct(temp)
all_pbp$roof <- coalesce(all_pbp$roof, 'dome')
all_pbp <- all_pbp %>% group_by(week) %>% mutate(temp = coalesce(temp,round(mean(temp, na.rm = T)))) %>% ungroup()

# timeouts, penalties & other stuff
all_pbp <- all_pbp %>% 
    mutate(
        pt_penalty = coalesce(ifelse(penalty_team == posteam, 1, 0),0),
        pt_timeout = coalesce(ifelse(timeout_team == posteam, 1, 0),0),
        pass_deep = coalesce(ifelse(pass_length == 'deep', 1, 0),0),
        pass_right = coalesce(ifelse(pass_location == 'right', 1, 0),0),
        pass_left = coalesce(ifelse(pass_location == 'left', 1, 0),0),
        pt_home = coalesce(ifelse(posteam_type == 'home', 1, 0),0),
    ) %>% 
    dplyr::select(-c(timeout_team, penalty_team, pass_length, pass_location))

##### Play Type #####
all_pbp$play_type <- ifelse(
    all_pbp$play_type == 'qb_spike' | all_pbp$qb_dropback == 1, 'pass',
    ifelse(
        all_pbp$play_type == 'run', paste(all_pbp$play_type, coalesce(all_pbp$run_location,'middle'), sep = '_'), all_pbp$play_type
    )
)

all_pbp %>% distinct(play_type)
colSums(is.na(all_pbp))

# fix down for this play
# all_pbp %>% filter(is.na(down)) %>% glimpse()
# all_pbp %>% filter(game_id =='2019_06_CAR_TB' & posteam == 'CAR' & pt_play_no %in% c(33,32,31)) %>% glimpse()
all_pbp$down <- coalesce(all_pbp$down,1)

##### Split data #####
all_pbp <- all_pbp %>% 
    filter(season >= 2016) %>% 
    dplyr::select(-run_location)

all_pbp_test <- all_pbp %>% 
    filter(season == 2022)

all_pbp_train <- all_pbp %>% 
    filter(season < 2022) %>% 
    filter(week %% 2 == 1 | 
        (season %% 2 == 1 & week %in% c(4, 8, 12, 16)) | 
        (season %% 2 == 0 & week %in% c(2, 6, 10, 14))
        )

all_pbp_valid <- all_pbp %>% 
    filter(season < 2022) %>% 
    filter(week %% 2 == 0 &
        !(season %% 2 == 1 & week %in% c(4, 8, 12, 16)) & 
        !(season %% 2 == 0 & week %in% c(2, 6, 10, 14))
        )

test_games <- all_pbp_test %>% distinct(game_id) %>% nrow()*2
train_games <- all_pbp_train %>% distinct(game_id) %>% nrow()*2
valid_games <- all_pbp_valid %>% distinct(game_id) %>% nrow()*2

print(paste("training games:", train_games + valid_games))
print(paste("validation split:", valid_games / (train_games + valid_games)))

print(paste("test_games:", test_games))
print(paste("test split" , test_games / (train_games + valid_games + test_games)))

##### Write #####
write.csv(all_pbp_train, 'data/nfl_plays_train_update.csv', row.names = F, na = "")
write.csv(all_pbp_valid, 'data/nfl_plays_valid_update.csv', row.names = F, na = "")
write.csv(all_pbp_test, 'data/nfl_plays_test_update.csv', row.names = F, na = "")