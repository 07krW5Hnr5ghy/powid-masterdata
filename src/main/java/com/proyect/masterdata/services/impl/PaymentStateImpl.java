package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.request.RequestPaymentStateSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentStateMapper;
import com.proyect.masterdata.repository.PaymentStateRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentState;
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
public class PaymentStateImpl implements IPaymentState {

    private final PaymentStateRepository paymentStateRepository;
    private final PaymentStateMapper paymentStateMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        PaymentState paymentState;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentState = paymentStateRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentState==null){
            throw new BadRequestExceptions(Constants.ErrorPaymentStateExists.toUpperCase());
        }

        try {
            paymentStateRepository.save(paymentStateMapper.paymentStateToName(RequestPaymentStateSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUser().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions{
        User datauser;
        List<PaymentState> paymentStates;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentStates = paymentStateRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!paymentStates.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorPaymentStateList.toUpperCase());
        }

        try {
            List<RequestPaymentStateSave> requestPaymentStateSaves = names.stream().map(data -> RequestPaymentStateSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            paymentStateRepository.saveAll(paymentStateMapper.listPaymentStateToListName(requestPaymentStateSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public PaymentStateDTO update(RequestPaymentState requestPaymentState) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        PaymentState paymentState;

        try{
            datauser = userRepository.findById(requestPaymentState.getUser().toUpperCase()).orElse(null);
            paymentState = paymentStateRepository.findById(requestPaymentState.getCode()).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentState==null){
            throw new BadRequestExceptions(Constants.ErrorPaymentState.toUpperCase());
        }

        paymentState.setName(requestPaymentState.getName().toUpperCase());
        paymentState.setUser(datauser.getUser().toUpperCase());
        paymentState.setStatus(requestPaymentState.isStatus());
        paymentState.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.save(paymentState));
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        PaymentState paymentState;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            paymentState = paymentStateRepository.findById(code).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(paymentState==null){
            throw new BadRequestExceptions(Constants.ErrorPaymentState.toUpperCase());
        }

        try {
            paymentState.setStatus(false);
            paymentState.setDateRegistration(new Date(System.currentTimeMillis()));
            paymentStateRepository.save(paymentState);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<PaymentStateDTO> listPaymentState() throws BadRequestExceptions{
        List<PaymentState> paymentStates = new ArrayList<>();
        try{
            paymentStates = paymentStateRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(paymentStates.isEmpty()){
            return Collections.emptyList();
        }
        return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStates);
    }

    @Override
    public List<PaymentStateDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return paymentStateMapper.listPaymentStateToListPaymentStateDTO(paymentStateRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public PaymentStateDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return paymentStateMapper.paymentStateToPaymentStateDTO(paymentStateRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
