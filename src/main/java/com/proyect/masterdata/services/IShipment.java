package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.ShipmentDTO;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IShipment {
    ResponseSuccess save(RequestShipment requestShipment, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    Page<ShipmentDTO> list(String serialPurchase, String user, String warehouse, String shipmentType, String sort, String sortColumn,
                                  Integer pageNumber, Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    Page<ShipmentDTO> listFalse(String serialPurchase, String user, String warehouse, String shipmentType, String sort, String sortColumn,
                                  Integer pageNumber, Integer pageSize) throws BadRequestExceptions,InternalErrorExceptions;
    List<ShipmentDTO> listShipment(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
