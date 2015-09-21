import jQuery from 'jquery';
import AppStore from '../stores/AppStore';

const engine_url = "http://localhost:8081";

function _updateBotsList(engine_response) {
    let bots = [];
    engine_response.ssBots.forEach((bot) => {
        bots.push(bot.botName);
    });
    AppStore.setBots(bots);
}

function _generateMatch(match) {
    const bots = AppStore.getBots();
    
    let result = {
        contesters: [],
        winner: -1,
        reason: 'TurnLimit',
        error_messages: [],
        log: undefined,
        url: undefined
    };
    
    let bot1_name = match.matchBots[0].botName;
    let bot2_name = match.matchBots[1].botName;
    
    let bot1_index = bots.findIndex((name) => { return name == bot1_name; });
    let bot2_index = bots.findIndex((name) => { return name == bot2_name; });
    
    if(bot1_index != undefined && bot2_index != undefined) {
        result.contesters.push(bot1_index);
        result.contesters.push(bot2_index);
        
        if(match.matchWinners.length == 1) {
            if(match.matchWinners[0].botName == bot1_name) result.winner = 0;
            else result.winner = 1;
        }
    } else {
        console.error("INTERNAL ERROR: Incoherent state returned!");
    }
    
    result.reason = match.matchGameOverType.tag;
    if(result.reason == 'Disqualification') {
        result.error_messages = match.matchGameOverType.contents;
    }
    
    return result;
}

function _getMatchLogURL(index) {
    return engine_url + "/replay/" + index;
}

function _updateMatches(engine_response) {
    let matches = [];
    engine_response.ssMatches.forEach((match, index) => {
        let m = _generateMatch(match);
        m.url = _getMatchLogURL(index);
        matches.push(m);
    });
    AppStore.setMatches(matches);
}

function _updateStateSuccess(engine_response) {
    AppStore.setMatches([]);
    _updateBotsList(engine_response);
    _updateMatches(engine_response);
}

function _updateMatchLog(index, engine_response) {
    AppStore.setMatchLog(index, engine_response);
}

export default {
    updateState() {
        jQuery.ajax({
            url: engine_url + "/state",
            data: {},
            type: "GET",
            dataType: "json",
            success: (response) => {
                _updateStateSuccess(response);
            },
            error: (xhr, status, error_thrown) => {
                console.error("Error: " + error_thrown);
                console.error("Status: " + status);
                console.dir(xhr);
            }
        });
    },
    
    loadMatchLog(index) {
        jQuery.ajax({
            url: engine_url + "/replay/" + index,
            data: {},
            type: "GET",
            dataType: "text",
            success: (response) => {
                _updateMatchLog(index, response);
            },
            error: (xhr, status, error_thrown) => {
                console.error("Error: " + error_thrown);
                console.error("Status: " + status);
                console.dir(xhr);
            }
        });
    }
};
