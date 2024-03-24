package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.UnitDTO;
import com.proyect.masterdata.dto.request.RequestUnit;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IUnit {
    ResponseSuccess save(RequestUnit requestUnit, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String unitType, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    List<UnitDTO> listUnit() throws BadRequestExceptions;
    List<UnitDTO> listUnitByType(String unitTypeName) throws BadRequestExceptions;
}
