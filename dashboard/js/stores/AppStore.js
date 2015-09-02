import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';
import ConsoleStore from './ConsoleStore';

const APP_CHANGE_EVENT = 'app_change';

const initialData = {
    bots: ['randy', 'rocky', 'scarlett', 'pepper'],
    matches: [
        {bot1: 0, bot2: 1, winner: 2},
        {bot1: 0, bot2: 2, winner: 1},
        {bot1: 0, bot2: 3, winner: 2},
        {bot1: 1, bot2: 2, winner: 1},
        {bot1: 2, bot2: 3, winner: 1}
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
};

var _AppStore = new AppStore();
export default _AppStore;

_AppStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {    
    default:
    }
});
