package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestPurchaseExcel;
import com.proyect.masterdata.dto.request.RequestShipmentExcel;
import com.proyect.masterdata.dto.request.RequestStockReturnExcel;
import com.proyect.masterdata.dto.request.RequestStockTransferExcel;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.util.concurrent.CompletableFuture;

public interface IExcel {
    CompletableFuture<ResponseSuccess> purchase(RequestPurchaseExcel requestPurchaseExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> shipment(RequestShipmentExcel requestShipmentExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockTransfer(RequestStockTransferExcel requestStockTransferExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockReturn(RequestStockReturnExcel requestStockReturnExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockReplenishment(Long orderId,MultipartFile multipartFile,String tokenUser) throws BadRequestExceptions;
}
