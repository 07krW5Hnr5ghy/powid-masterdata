package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ShipmentDTO;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IShipment {
    ResponseSuccess save(String serial, String warehouse, List<RequestShipment> requestShipmentList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<ShipmentDTO> list(String serial, String user, String warehouse, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}