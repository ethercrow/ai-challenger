import React from 'react';

class Ladder extends React.Component {
    render() {
        return (
            <div id="left_column">
                <table id="ladder">
                    <caption>LADDER</caption>

                    <thead>
                        <tr><th></th><th>Name</th><th>Win/Lost</th></tr>
                    </thead>

                    <tbody>
                        <tr><td>1</td><td>randy</td><td>1/2</td></tr>
                        <tr><td>2</td><td>rocky</td><td>2/1</td></tr>
                        <tr><td>3</td><td>scarlett</td><td>1/2</td></tr>
                        <tr><td>4</td><td>pepper</td><td>2/1</td></tr>
                    </tbody>
                </table>
            </div>
        );
    }
};

export default Ladder;
