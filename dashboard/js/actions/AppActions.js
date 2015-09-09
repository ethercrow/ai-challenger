import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

export default {
    openMatchWindow(bot1_index, bot2_index) {
        Dispatcher.dispatch({
            type: Constants.APP_OPEN_MATCH_WINDOW,
            data: {
                bot1_index: bot1_index,
                bot2_index: bot2_index
            }
        });
    },
    
    closeMatchWindow() {
        Dispatcher.dispatch({ type: Constants.APP_CLOSE_MATCH_WINDOW });
    }
};
