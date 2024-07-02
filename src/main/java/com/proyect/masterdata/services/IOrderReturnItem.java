package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderReturnItem {
    CompletableFuture<ResponseSuccess> save(Long orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseDelete> delete(Long orderId,String supplierProductSerial,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> activate(Long orderId,String supplierProductSerial,String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> update(Long orderId,String supplierProductSerial,Integer quantity,String tokenUser) throws InternalErrorExceptions;
    CompletableFuture<List<OrderReturnItemDTO>> list(String user,Long orderId) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnItemDTO>> listPagination(
            String user,
            List<Long> orderIds,
            List<String> products,
            List<String> supplierProducts,
            List<String> warehouses,
            List<String> orderReturnTypes,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
    CompletableFuture<Page<OrderReturnItemDTO>> listFalse(
            String user,
            List<Long> orderIds,
            List<String> products,
            List<String> supplierProducts,
            List<String> warehouses,
            List<String> orderReturnTypes,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions;
}
