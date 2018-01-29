import React from 'react';

export default ({user, loginUser}: { user: string | null, loginUser: any }) => {

    const loginComponent = user ?
        <div>{user + ' logged in'}</div>
        :
        <div>
            <label>Please log in: </label>
            <input type="text" onBlur={e => loginUser(e.currentTarget.value)}/>
        </div>;

    return <div><h3>User Login</h3>{loginComponent}</div>;
}
