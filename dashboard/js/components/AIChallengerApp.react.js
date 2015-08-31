import React from 'react';
import Header from './Header.react'
import Ladder from './Ladder.react'
import Console from './Console.react'

class AIChallengerApp extends React.Component {
    render() {
        return (
            <div>
                <Header/>
                <Ladder/>
                <Console/>
            </div>
        );
    }
};

export default AIChallengerApp;
