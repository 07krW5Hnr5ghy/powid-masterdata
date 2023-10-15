package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Channel;
import com.proyect.masterdata.domain.Payment;
import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.request.RequestPaymentSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentMapper;
import com.proyect.masterdata.repository.ChannelRepository;
import com.proyect.masterdata.repository.PaymentRepository;
import com.proyect.masterdata.repository.PaymentRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPayment;
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
public class PaymentImpl implements IPayment {
    private final PaymentRepository paymentRepository;
    private final UserRepository userRepository;
    private final ChannelRepository channelRepository;
    private final PaymentRepositoryCustom paymentRepositoryCustom;
    private final PaymentMapper paymentMapper;
    @Override
    public ResponseSuccess save(RequestPaymentSave requestPaymentSave, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Channel channel;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            channel = channelRepository.findByName(requestPaymentSave.getChannel().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(channel==null){
            throw new BadRequestExceptions("Canal no existe");
        }
        try {
            paymentRepository.save(Payment.builder()
                    .totalPayment(requestPaymentSave.getTotalPayment())
                    .discount(requestPaymentSave.getDiscount())
                    .month(requestPaymentSave.getMonth().toUpperCase())
                    .channel(channel)
                    .idChannel(channel.getId())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .build()
            );
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
    public ResponseSuccess saveAll(List<RequestPaymentSave> requestPaymentSaveList, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Channel> channelList;
        try{
            existsUser = userRepository.existsById(user.toUpperCase());
            channelList = channelRepository.findByNameIn(requestPaymentSaveList.stream().map(payment -> payment.getChannel().toUpperCase()).toList());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(!existsUser){
            throw new BadRequestExceptions("Usuario no existe");
        }
        if(channelList.size() != requestPaymentSaveList.size()){
            throw new BadRequestExceptions("Canal no existe");
        }
        try{
            paymentRepository.saveAll(requestPaymentSaveList.stream().map(payment -> Payment.builder()
                    .totalPayment(payment.getTotalPayment())
                    .month(payment.getMonth().toUpperCase())
                    .discount(payment.getDiscount())
                    .channel(channelRepository.findByName(payment.getChannel().toUpperCase()))
                    .idChannel(channelRepository.findByName(payment.getChannel().toUpperCase()).getId())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .build()
            ).toList());
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
    public Page<PaymentDTO> list(Double totalPayment, String month, String channel, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Payment> paymentPage;
        Channel channelData;
        try{
            if(channel != null){
                channelData = channelRepository.findByName(channel.toUpperCase());
                paymentPage = paymentRepositoryCustom.searchForPayment(totalPayment,month,channelData.getId(),sort,sortColumn,pageNumber,pageSize);
            }else{
                paymentPage = paymentRepositoryCustom.searchForPayment(totalPayment,month,null,sort,sortColumn,pageNumber,pageSize);
            }
        }catch (RuntimeException e){
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(paymentPage.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }
        List<PaymentDTO> paymentDTOList = paymentPage.getContent().stream().map(payment -> PaymentDTO.builder()
                .totalPayment(payment.getTotalPayment())
                .month(payment.getMonth())
                .discount(payment.getDiscount())
                .channel(payment.getChannel().getName())
                .build()).toList();
        return new PageImpl<>(paymentDTOList,
                paymentPage.getPageable(),paymentPage.getTotalElements());
    }

}
