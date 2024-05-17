package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestPurchaseExcel;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.util.concurrent.CompletableFuture;

public interface IExcel {
    CompletableFuture<ResponseSuccess> purchase(RequestPurchaseExcel requestPurchaseExcel,MultipartFile multipartFile, String tokenUser) throws BadRequestExceptions;
}
