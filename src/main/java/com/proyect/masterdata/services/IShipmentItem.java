package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IShipmentItem {
    ResponseSuccess save(String serial, String warehouse, List<RequestShipmentItem> requestShipmentItemList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    Page<ShipmentItemDTO> list(String serial, String user, String warehouse, String sort, String sortColumn,
                               Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
}