package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IProductPicture {
    List<String> uploadPicture(List<MultipartFile> pictures, UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<List<String>> uploadPictureAsync(List<File> pictures, UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
