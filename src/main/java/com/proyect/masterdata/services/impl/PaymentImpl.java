package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.dto.request.RequestPaymentSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PaymentRepository;
import com.proyect.masterdata.services.IPayment;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentImpl implements IPayment {
    private final PaymentRepository paymentRepository;
    @Override
    public ResponseSuccess save(RequestPaymentSave requestPaymentSave, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean existsPayment;
        return null;
    }
}
