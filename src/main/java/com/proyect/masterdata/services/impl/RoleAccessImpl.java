package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IRoleAccess;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class RoleAccessImpl implements IRoleAccess {

    @Override
    public ResponseSuccess save(String role, String Access, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'save'");
    }

}
