import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import ConsoleStore from './ConsoleStore';

const APP_CHANGE_EVENT = 'app_change';

const initialData = {
    bots: ['randy', 'rocky', 'scarlett', 'pepper'],
    matches: [
        {contesters: [0, 1], winner: 1},
        {contesters: [0, 2], winner: 0},
        {contesters: [0, 3], winner: 1},
        {contesters: [1, 2], winner: 0},
        {contesters: [1, 3], winner: 1},
        {contesters: [2, 3], winner: 0}
    ]
};
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
    
    executeCommand(command, args) {
        switch(command) {
        case 'help':
            this.executeHelp();
            break;
        
        case 'list':
            this.executeList(args);
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
};

var _AppStore = new AppStore();
export default _AppStore;
