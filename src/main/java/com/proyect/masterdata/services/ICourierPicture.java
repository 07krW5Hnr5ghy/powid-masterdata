package com.proyect.masterdata.services;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface ICourierPicture {
    public List<String> uploadPicture(List<MultipartFile> pictures, Long orderId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
