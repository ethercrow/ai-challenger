import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import EngineConnect from '../utils/EngineConnect';

const APP_CHANGE_EVENT = 'app_change';

const initialData = {
    display_match: undefined,
    
    bots: [],
    matches: []
};

let data = initialData;

class AppStore extends EventEmitter {
    constructor() {
        super();
        
        EngineConnect.updateState();
        window.setInterval(EngineConnect.updateState, 60000);
    }
    
    emitChange() {
        this.emit(APP_CHANGE_EVENT);
    }
    
    onChange(callback) {
        this.on(APP_CHANGE_EVENT, callback);
    }
    
    off(callback) {
        this.removeListener(APP_CHANGE_EVENT, callback);
    }
    
    openMatchWindow(bot1_index, bot2_index) {
        const match_index = data.matches.findIndex((match) => {
            if(match.contesters[0] == bot1_index && match.contesters[1] == bot2_index) {
                return true;
            } else {
                if(match.contesters[0] == bot2_index && match.contesters[1] == bot1_index) {
                    return true;
                } 
            }
        });
        if(match_index == -1) {
            ConsoleStore.writeLine("ERROR: The match between '" + data.bots[bot1_index] + "' and '" + data.bots[bot2_index] + "' wasn't found.");
            return;
        }
        
        if(data.matches[match_index].log == undefined) {
            EngineConnect.loadMatchLog(match_index);
        }
        data.display_match = data.matches[match_index];
    }
    
    closeMatchWindow() {
        data.display_match = undefined;
    }
    
    getDisplayMatchWindow() {
        return data.display_match != undefined;
    }
    
    getMatchWindowMatch() {
        return data.display_match;
    }
    
    getBots() {
        return data.bots;
    }
    
    setBots(bots) {
        data.bots = bots;
        this.emitChange();
    }
    
    getBotNumVictories(bot_index) {
        return data.matches.reduce((num_victories, match) => {
            return num_victories + ((match.contesters[match.winner] == bot_index)?1:0);
        }, 0);
    }
    
    getBotNumDraws(bot_index) {
        return data.matches.reduce((num_draws, match) => {
            if((match.contesters[0] == bot_index) || (match.contesters[1] == bot_index)) {
                return num_draws + ((match.winner == -1)?1:0);
            } else return num_draws;
        }, 0);
    }

    getBotNumDefeats(bot_index) {
        return data.matches.reduce((num_defeats, match) => {
            if((match.contesters[0] == bot_index) || (match.contesters[1] == bot_index)) {
                return num_defeats + ((match.winner != -1 && match.contesters[match.winner] != bot_index)?1:0);
            } else return num_defeats;
        }, 0);
    }
    
    getMatchTable() {
        let match_table = new Array();
        for(let i = 0; i < data.bots.length; i++) {
            match_table.push(new Array(data.bots.length));
        }
        
        data.matches.forEach((match) => {
            let reason = '';
            switch(match.reason) {
            case 'Elimination':
                reason = 'E';
                break;
            
            case 'TurnLimit':
                reason = 'TL';
                break;
            
            case 'Disqualification':
                reason = 'DQ';
                break;
            }

            match_table[match.contesters[0]][match.contesters[1]] = 'DWL'[match.winner + 1] + ':' + reason;
            match_table[match.contesters[1]][match.contesters[0]] = 'DLW'[match.winner + 1] + ':' + reason;
        });
        
        return match_table;
    }
    
    setMatches(matches) {
        data.matches = matches;
        this.emitChange();
    }
    
    setMatchLog(index, log) {
        data.matches[index].log = log;
    }
};

var _AppStore = new AppStore();
export default _AppStore;

_AppStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
    case ActionTypes.APP_OPEN_MATCH_WINDOW:
        _AppStore.openMatchWindow(action.data.bot1_index, action.data.bot2_index);
        _AppStore.emitChange();
        break;
    
    case ActionTypes.APP_CLOSE_MATCH_WINDOW:
        _AppStore.closeMatchWindow();
        _AppStore.emitChange();
        break;
    
    default:
    }
});
