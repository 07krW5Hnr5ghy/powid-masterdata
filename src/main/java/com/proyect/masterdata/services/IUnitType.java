package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.UnitTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IUnitType {
    public ResponseSuccess save(String name,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseSuccess saveAll(List<String> names, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public List<UnitTypeDTO> listUnitType() throws BadRequestExceptions;
}
