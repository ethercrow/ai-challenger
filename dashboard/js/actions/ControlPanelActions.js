import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

export default {
    reset(max_turns) {
        Dispatcher.dispatch({ 
            type: Constants.CONTROL_PANEL_RESET,
            data: {
                max_turns: max_turns
            }
        });
    },
    
    changeTurn(current_turn) {
        Dispatcher.dispatch({ 
            type: Constants.CONTROL_PANEL_CHANGE_TURN,
            data: {
                current_turn: current_turn
            }
        });
    },
    
    startPlaying() {
        Dispatcher.dispatch({ type: Constants.CONTROL_PANEL_START_PLAYING });
    },
    
    stopPlaying() {
        Dispatcher.dispatch({ type: Constants.CONTROL_PANEL_STOP_PLAYING });
    }
};
