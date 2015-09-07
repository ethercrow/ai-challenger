import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import ConsoleStore from './ConsoleStore';

import initialData from '../constants/AppStoreInitialData';

const APP_CHANGE_EVENT = 'app_change';

let data = initialData;

class AppStore extends EventEmitter {
    emitChange() {
        this.emit(APP_CHANGE_EVENT);
    }
    
    onChange(callback) {
        this.on(APP_CHANGE_EVENT, callback);
    }
    
    off(callback) {
        this.removeListener(APP_CHANGE_EVENT, callback);
    }
    
    displayMatchWindow(match) {
        data.display_match = match;
        this.emitChange();
    }
    
    closeMatchWindow() {
        data.display_match = undefined;
    }
    
    executeCommand(command, args) {
        switch(command) {
        case 'help':
            this.executeHelp();
            break;
        
        case 'list':
            this.executeList(args);
            break;
        
        case 'show':
            this.executeShow(args);
            break;
        
        case 'table':
            this.executeTable(args);
            break;
        
        default:
            ConsoleStore.writeLine("ERROR: Unknown command '" + command + "'.");
        }
    }
    
    executeHelp() {
        ConsoleStore.writeLine("AI Challenger accepts the following commands:");
        ConsoleStore.writeLine("    * help - Print this message;")
        ConsoleStore.writeLine("    * list - Print the list of currently loaded bots;");
        ConsoleStore.writeLine("    * show <bot> - Print the match statistics for the bot <bot>;");
        ConsoleStore.writeLine("    * show <bot1> <bot2> - Show the match between <bot1> and <bot2>;")
        ConsoleStore.writeLine("    * table - Print the general match table.")
    }
    
    executeList(args) {
        if(args.length != 0) {
            ConsoleStore.writeLine("ERROR: The command 'list' takes no argument.");
        } else {
            ConsoleStore.writeLine("Currently registered bots are:");
            data.bots.forEach((name) => {
                ConsoleStore.writeLine(name);
            });
        }
    }
    
    executeTable(args) {
        if(args.length != 0) {
            ConsoleStore.writeLine("ERROR: The command 'table' takes no argument.");
        } else {
            const match_table = this.getMatchTable();
            
            // Computing column width
            const column_width = data.bots.reduce((max_length, bot_name) => {
                if(bot_name.length > max_length) {
                    return bot_name.length;
                } else {
                    return max_length;
                }
            }, 0) + 2;
            
            // Printing table header
            let str = '';
            for(let i = 0; i < column_width; i++) str += ' ';
            data.bots.forEach((bot_name) => {
                str += '| ';
                str += bot_name;
                for(let i = column_width - 2 - bot_name.length; i >= 0; i--) {
                    str += ' ';
                }
            });
            ConsoleStore.writeLine(str);
            
            // Printing separator
            str = '';
            for(let i = 0; i < column_width; i++) {
                str += '-'
            }
            for(let i = 0; i < data.bots.length; i++) {
                str += '|';
                for(let j = 0; j < column_width; j++) {
                    str += '-';
                }
            }
            ConsoleStore.writeLine(str);
            
            // Printing lines
            data.bots.forEach((bot_name, bot_index) => {
                str = ' ';
                str += bot_name;
                for(let i = column_width - 2 - bot_name.length; i >= 0; i--) {
                    str += ' ';
                }
                
                for(let i = 0; i < data.bots.length; i++) {
                    str += '| ';
                    str += (match_table[bot_index][i] == undefined)?' ':match_table[bot_index][i];
                    for(let j = column_width - 3; j >= 0; j--) {
                        str += ' ';
                    }
                }
                
                ConsoleStore.writeLine(str);
            });
        }
    }
    
    executeShowWith1Arg(bot_name) {
        const bot_index = data.bots.indexOf(bot_name);
        if(bot_index == -1) {
            ConsoleStore.writeLine("ERROR: The bot '" + bot_name + "' doesn't exist!");
        }
        
        ConsoleStore.writeLine("The bot " + bot_name + " won " + this.getBotNumVictories(bot_index) 
        + " times and lost " + this.getBotNumDefeats(bot_index) + " times.");
        
        let won_against = new Array();
        let lost_against = new Array();
        data.matches.forEach((match) => {
            if(match.contesters[0] == bot_index) {
                if(match.winner == 0) won_against.push(data.bots[match.contesters[1]]);
                else lost_against.push(data.bots[match.contesters[1]]);
            } else {
                if(match.contesters[1] == bot_index) {
                    if(match.winner == 1) won_against.push(data.bots[match.contesters[0]]);
                    else lost_against.push(data.bots[match.contesters[0]]);
                }
            }
        });
        
        ConsoleStore.writeLine("Won against: " + won_against.reduce((str, name) => {
            return str + name + ', ';
        }, ""));
        ConsoleStore.writeLine("Lost against: " + lost_against.reduce((str, name) => {
            return str + name + ', ';
        }, ""));
    }
    
    executeShowWith2Args(bot_name1, bot_name2) {
        const bot_index1 = data.bots.indexOf(bot_name1);
        const bot_index2 = data.bots.indexOf(bot_name2);
        if(bot_index1 == -1) {
            ConsoleStore.writeLine("ERROR: The bot '" + bot_name1 + "' doesn't exist!");
            return;
        }
        if(bot_index2 == -1) {
            ConsoleStore.writeLine("ERROR: The bot '" + bot_name2 + "' doesn't exist!");
            return;
        }
        
        const match = data.matches.find((match) => {
            if(match.contesters[0] == bot_index1 && match.contesters[1] == bot_index2) {
                return true;
            } else {
                if(match.contesters[0] == bot_index2 && match.contesters[1] == bot_index1) {
                    return true;
                } 
            }
        });
        if(match == undefined) {
            ConsoleStore.writeLine("ERROR: The match between '" + bot_name1 + "' and '" + bot_name2 + "' wasn't found.");
            return;
        }
        
        this.displayMatchWindow(match);
    }
    
    executeShow(args) {
        switch(args.length) {
        case 1:
            this.executeShowWith1Arg(args[0]);
            break;
        
        case 2:
            this.executeShowWith2Args(args[0], args[1]);
            break;
        
        default:
            ConsoleStore.writeLine("ERROR: The command 'show' accept one or two arguments.");
        }
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
    
    getBotNumVictories(bot_index) {
        return data.matches.reduce((num_victories, match) => {
            return num_victories + ((match.contesters[match.winner] == bot_index)?1:0);
        }, 0);
    }
    
    getBotNumDefeats(bot_index) {
        return data.matches.reduce((num_defeats, match) => {
            if((match.contesters[0] == bot_index) || (match.contesters[1] == bot_index)) {
                return num_defeats + ((match.contesters[match.winner] != bot_index)?1:0);
            } else return num_defeats;
        }, 0);
    }
    
    getMatchTable() {
        let match_table = new Array();
        for(let i = 0; i < data.bots.length; i++) {
            match_table.push(new Array(data.bots.length));
        }
        
        data.matches.forEach((match) => {
            match_table[match.contesters[0]][match.contesters[1]] = (match.winner == 0)?'W':'L';
            match_table[match.contesters[1]][match.contesters[0]] = (match.winner == 1)?'W':'L';
        });
        
        return match_table;
    }
};

var _AppStore = new AppStore();
export default _AppStore;

_AppStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
    case ActionTypes.APP_CLOSE_MATCH_WINDOW:
        _AppStore.closeMatchWindow();
        _AppStore.emitChange();
        break;
    
    default:
    }
});
