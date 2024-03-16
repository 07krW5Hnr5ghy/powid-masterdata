package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IPurchaseDocument {
    public ResponseSuccess save(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    public ResponseDelete delete(String name,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    public List<String> list() throws BadRequestExceptions;
}
