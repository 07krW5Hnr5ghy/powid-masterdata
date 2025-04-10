package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrdering {
    ResponseSuccess save(RequestOrderSave requestOrderSave, MultipartFile[] receipts, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestOrderSave requestOrderSave, MultipartFile[] receipts, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<OrderDTO>> list(
            UUID orderId,
            String user,
            String seller,
            String customer,
            String customerPhone,
            String instagram,
            List<String> departments,
            List<String> provinces,
            List<String> districts,
            List<String> saleChannels,
            Boolean receiptFlag,
            Boolean deliveryFlag,
            List<String> deliveryPoints,
            List<String> orderStates,
            List<String> couriers,
            String paymentState,
            String paymentMethod,
            String managementType,
            String storeName,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<OrderDTO>> listOrder(String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess update(UUID orderId, RequestOrderUpdate requestOrderUpdate,MultipartFile[] receipts,MultipartFile[] courierPictures,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> updateAsync(
            UUID orderId,
            RequestOrderUpdate requestOrderUpdate,
            MultipartFile[] receipts,
            MultipartFile[] courierPictures,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<OrderDTO> selectOrder(UUID orderId,String username) throws InternalErrorExceptions,BadRequestExceptions;
    CompletableFuture<ResponseSuccess> uploadPhotos(UUID orderId, MultipartFile[] receipts,MultipartFile[] courierPictures,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
