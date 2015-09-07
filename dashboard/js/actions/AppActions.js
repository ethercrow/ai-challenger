import Constants from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

export default {
    closeMatchWindow() {
        Dispatcher.dispatch({ type: Constants.APP_CLOSE_MATCH_WINDOW });
    }
};
