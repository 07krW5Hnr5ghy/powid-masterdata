package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

public interface IOrderPaymentReceipt {
    public List<String> uploadReceipt(List<MultipartFile> receipts, Long orderId,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
}
