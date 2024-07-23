package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Shipment;
import com.proyect.masterdata.domain.ShipmentItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IShipmentItem {
    ShipmentItem save(Shipment shipment, String warehouse, RequestShipmentItem requestShipmentItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ShipmentItem> saveAsync(Shipment shipment, String warehouse, RequestShipmentItem requestShipmentItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseDelete> delete(String serial,String supplierProduct, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> activate(String serial,String supplierProduct, String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<ShipmentItemDTO>> list(
            String user,
            List<String> shipments,
            List<String> warehouses,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<ShipmentItemDTO>> listShipmentItem(String user,Long id) throws InternalErrorExceptions,BadRequestExceptions;
}