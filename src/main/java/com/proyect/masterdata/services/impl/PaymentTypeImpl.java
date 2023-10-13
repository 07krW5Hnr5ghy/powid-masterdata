package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentTypeDTO;
import com.proyect.masterdata.dto.request.RequestPaymentTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentTypeMapper;
import com.proyect.masterdata.repository.PaymentTypeRepository;
import com.proyect.masterdata.repository.PaymentTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class PaymentTypeImpl implements IPaymentType {
    private final PaymentTypeRepository paymentTypeRepository;
    private final UserRepository userRepository;
    private final PaymentTypeRepositoryCustom paymentTypeRepositoryCustom;
    private final PaymentTypeMapper paymentTypeMapper;
    @Override
    public ResponseSuccess save(String type,String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean existsType;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            existsType = paymentTypeRepository.existsByType(type.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(existsType){
            throw new BadRequestExceptions("Tipo de pago ya existe");
        }
        try{
            paymentTypeRepository.save(PaymentType.builder()
                            .type(type.toUpperCase())
                            .dateRegistration(new Date(System.currentTimeMillis()))
                            .status(true)
                            .user(user.toUpperCase())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<PaymentType> paymentTypeList;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            paymentTypeList = paymentTypeRepository.findByTypeIn(names);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(!paymentTypeList.isEmpty()){
            throw new BadRequestExceptions("Tipo de pago ya existe");
        }
        try{
            paymentTypeRepository.saveAll(paymentTypeMapper.listPaymentTypeToName(names.stream().map(name -> RequestPaymentTypeSave.builder()
                    .type(name.toUpperCase())
                    .user(user.toUpperCase())
                    .build()).toList()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public PaymentTypeDTO update(RequestPaymentTypeSave requestPaymentTypeSave) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        PaymentType paymentType;
        try{
            existsUser = userRepository.existsById(requestPaymentTypeSave.getUser().toUpperCase());
            paymentType = paymentTypeRepository.findByType(requestPaymentTypeSave.getType().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(paymentType==null){
            throw new BadRequestExceptions("Tipo de pago no existe");
        }
        try{
            paymentTypeRepository.save(PaymentType.builder()
                            .type(requestPaymentTypeSave.getType().toUpperCase())
                            .user(requestPaymentTypeSave.getUser().toUpperCase())
                            .status(true)
                            .dateRegistration(new Date(System.currentTimeMillis()))
                    .build());
            return PaymentTypeDTO.builder()
                    .type(requestPaymentTypeSave.getType().toUpperCase())
                    .user(requestPaymentTypeSave.getUser().toUpperCase())
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(String type, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        PaymentType paymentType;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            paymentType = paymentTypeRepository.findByType(type.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(paymentType==null){
            throw new BadRequestExceptions("Tipo de pago no existe");
        }
        try{
            paymentType.setStatus(false);
            paymentType.setDateRegistration(new Date(System.currentTimeMillis()));
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<PaymentTypeDTO> list(String type,String user,String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<PaymentType> paymentTypePage;
        try{
            paymentTypePage = paymentTypeRepositoryCustom.searchForPaymentType(type,user,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(paymentTypePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(paymentTypeMapper.listPaymentTypeToListPaymentTypeDTO(paymentTypePage.getContent()),
                paymentTypePage.getPageable(),paymentTypePage.getTotalElements());
    }

    @Override
    public Page<PaymentTypeDTO> listStatusFalse(String type, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<PaymentType> paymentTypePage;
        try{
            paymentTypePage = paymentTypeRepositoryCustom.searchForPaymentType(type,user,sort,sortColumn,pageNumber,pageSize,false);
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(paymentTypePage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(paymentTypeMapper.listPaymentTypeToListPaymentTypeDTO(paymentTypePage.getContent()),
                paymentTypePage.getPageable(),paymentTypePage.getTotalElements());
    }
}
