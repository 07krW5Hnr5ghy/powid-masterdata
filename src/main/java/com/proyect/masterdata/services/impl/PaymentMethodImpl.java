package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.request.RequestPaymentMethodSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentMethod;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentMethodImpl implements IPaymentMethod {

    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        PaymentMethod paymentMethod;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentMethod = paymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentMethod!=null){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethodExists.toUpperCase());
        }

        try {
            paymentMethodRepository.save(paymentMethodMapper.paymentMethodToName(RequestPaymentMethodSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUser().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions{
        User datauser;
        List<PaymentMethod> paymentMethods;
        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentMethods = paymentMethodRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!paymentMethods.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethodList.toUpperCase());
        }

        try {
            List<RequestPaymentMethodSave> paymentMethodSaves = names.stream().map(data -> RequestPaymentMethodSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            paymentMethodRepository.saveAll(paymentMethodMapper.listPaymentMethodToListName(paymentMethodSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public PaymentMethodDTO update(RequestPaymentMethod requestPaymentMethod) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        PaymentMethod paymentMethod;

        try{
            datauser = userRepository.findById(requestPaymentMethod.getUser().toUpperCase()).orElse(null);
            paymentMethod = paymentMethodRepository.findById(requestPaymentMethod.getCode()).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentMethod==null){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
        }

        paymentMethod.setName(requestPaymentMethod.getName().toUpperCase());
        paymentMethod.setUser(datauser.getUser().toUpperCase());
        paymentMethod.setStatus(requestPaymentMethod.isStatus());
        paymentMethod.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return paymentMethodMapper.paymentMethodToPaymentMethodDTO(paymentMethodRepository.save(paymentMethod));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        PaymentMethod paymentMethod;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentMethod = paymentMethodRepository.findById(code).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentMethod==null){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
        }

        try {
            paymentMethod.setStatus(false);
            paymentMethod.setDateRegistration(new Date(System.currentTimeMillis()));
            paymentMethodRepository.save(paymentMethod);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions{
        List<PaymentMethod> paymentMethods = new ArrayList<>();
        try{
            paymentMethods = paymentMethodRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(paymentMethods.isEmpty()){
            return Collections.emptyList();
        }
        return paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethods);
    }

    @Override
    public List<PaymentMethodDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethodRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentMethodDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return paymentMethodMapper.paymentMethodToPaymentMethodDTO(paymentMethodRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
