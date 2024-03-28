package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.Shipment;
import com.proyect.masterdata.domain.ShipmentItem;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IShipmentItem {
    ShipmentItem save(Shipment shipment, Purchase purchase, String warehouse, RequestShipmentItem requestShipmentItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    Page<ShipmentItemDTO> list(String purchaseSerial, String user, String supplierProductSerial, String sort, String sortColumn,
                               Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    List<ShipmentItemDTO> listShipmentItem(String user,Long id) throws InternalErrorExceptions,BadRequestExceptions;
}