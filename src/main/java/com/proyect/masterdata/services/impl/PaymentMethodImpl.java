package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.request.RequestPaymentMethodSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PaymentMethodRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IPaymentMethod;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class PaymentMethodImpl implements IPaymentMethod {

    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentMethodMapper paymentMethodMapper;
    private final UserRepository userRepository;
    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            paymentMethodRepository.save(paymentMethodMapper.paymentMethodToName(name.toUpperCase(),user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
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
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public PaymentMethodDTO update(RequestPaymentMethod requestPaymentMethod) throws BadRequestExceptions {
        try {
            requestPaymentMethod.setName(requestPaymentMethod.getName().toUpperCase());
            requestPaymentMethod.setUser(requestPaymentMethod.getUser().toUpperCase());
            PaymentMethod updatedPaymentMethod = paymentMethodMapper.requestPaymentMethodToPaymentMethod(requestPaymentMethod);
            updatedPaymentMethod.setDateRegistration(new Date(System.currentTimeMillis()));
            PaymentMethod paymentMethod = paymentMethodRepository.save(updatedPaymentMethod);
            return paymentMethodMapper.paymentMethodToPaymentMethodDTO(paymentMethod);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            paymentMethodRepository.deleteByIdAndUser(code,user);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            codes.stream().forEach(data -> {
                paymentMethodRepository.deleteByIdAndUser(data,user);
            });
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<PaymentMethodDTO> list() throws BadRequestExceptions{
        try {
            return paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethodRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
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

    @Override
    public PaymentMethodDTO findByName(String name) throws BadRequestExceptions{
        try {
            return paymentMethodMapper.paymentMethodToPaymentMethodDTO(paymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<PaymentMethodDTO> findByUser(String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            return paymentMethodMapper.listPaymentMethodToListPaymentMethodDTO(paymentMethodRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
