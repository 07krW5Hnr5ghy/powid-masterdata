package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IOrderPaymentReceipt {
    CompletableFuture<List<String>> uploadReceipt(MultipartFile[] receipts, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<String>> uploadReceiptAsync(MultipartFile[] receipts, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<String>> uploadReceiptFileAsync(List<File> fileList, Long orderId, String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
