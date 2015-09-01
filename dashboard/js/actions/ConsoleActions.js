import ConsoleStore from '../stores/ConsoleStore';
import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

export default {
    consoleCharacterTyped(character) {
        Dispatcher.dispatch({ 
            type: Constants.CONSOLE_CHARACTER_TYPED,
            data: character 
        });
    },
    
    consoleBackspaceTyped() {
        Dispatcher.dispatch({ type: Constants.CONSOLE_BACKSPACE_TYPED });
    },
    
    consoleEnterTyped() {
        Dispatcher.dispatch({ type: Constants.CONSOLE_ENTER_TYPED });
    }
};
