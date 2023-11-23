package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseLogin;

public interface IAuthentication {
    public ResponseLogin loginUser(String username, String password);
}
