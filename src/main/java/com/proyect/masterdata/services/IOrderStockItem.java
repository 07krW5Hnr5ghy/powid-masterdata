package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IOrderStockItem {
    ResponseSuccess save(OrderStock orderStock, RequestOrderStockItem requestOrderStockItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    Page<OrderStockItemDTO> list(String user, Long orderId, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Page<OrderStockItemDTO> listFalse(String user, Long orderId, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    Boolean checkWarehouseItemStock(Long orderId, Warehouse warehouse, RequestOrderStockItem requestOrderStockItem) throws InternalErrorExceptions,BadRequestExceptions;
    List<OrderStockItemDTO> listOrderStockItem(String user,Long orderId) throws BadRequestExceptions,InternalErrorExceptions;
    List<OrderStockItemDTO> listOrderStockItemFalse(String user,Long orderId) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long orderId,String supplierProductSerial,String tokenUser) throws BadRequestExceptions,InternalErrorExceptions;

}
