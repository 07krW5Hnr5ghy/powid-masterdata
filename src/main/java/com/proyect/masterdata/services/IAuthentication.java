package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestOnboarding;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IAuthentication {
    public ResponseLogin loginUser(String username, String password);

    public ResponseSuccess registerUser(RequestOnboarding requestOnboarding)
            throws InternalErrorExceptions, BadRequestExceptions;
}
