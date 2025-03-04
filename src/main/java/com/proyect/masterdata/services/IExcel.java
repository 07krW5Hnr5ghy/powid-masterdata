package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestSupplyOrderExcel;
import com.proyect.masterdata.dto.request.RequestStockReturnExcel;
import com.proyect.masterdata.dto.request.RequestStockTransferExcel;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IExcel {
    CompletableFuture<ResponseSuccess> purchase(RequestSupplyOrderExcel requestSupplyOrderExcel, MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockTransfer(RequestStockTransferExcel requestStockTransferExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockReturn(RequestStockReturnExcel requestStockReturnExcel,MultipartFile multipartFile) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> stockReplenishment(
            UUID orderId,
            MultipartFile multipartFile,
            String tokenUser) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> orderStock(
            UUID orderId,
            String warehouseName,
            MultipartFile multipartFile,
            String tokenUser) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> orderReturn(
            UUID orderId,
            MultipartFile multipartFile,
            String tokenUser) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> product(MultipartFile multipartFile,String tokenUser) throws BadRequestExceptions;
    CompletableFuture<ResponseSuccess> supplierProduct(MultipartFile multipartFile,String tokenUser) throws BadRequestExceptions;
    String getExcelColumnReference(Character character,Integer index);
    CompletableFuture<ResponseSuccess> model(MultipartFile multipartFile,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
}
