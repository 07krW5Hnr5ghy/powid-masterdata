package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannelSave;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SaleChannelMapper;
import com.proyect.masterdata.repository.SaleChannelRepository;
import com.proyect.masterdata.services.ISaleChannel;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SaleChannelImpl implements ISaleChannel {
    private final SaleChannelRepository saleChannelRepository;
    private final SaleChannelMapper saleChannelMapper;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        try {
            saleChannelRepository.save(saleChannelMapper.saleChannelToName(name.toUpperCase(),user.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestSaleChannelSave> requestSaleChannelSaveList) throws BadRequestExceptions{
        try {
            saleChannelRepository.saveAll(saleChannelMapper.listRequestCreateSaleChannelToListSaleChannel(requestSaleChannelSaveList)
                    .stream()
                    .map(
                            c -> {
                                SaleChannel saleChannel = new SaleChannel();
                                saleChannel.setName(c.getName().toUpperCase());
                                saleChannel.setStatus(c.getStatus());
                                saleChannel.setUser(c.getUser().toUpperCase());
                                return saleChannel;
                            }
                    ).collect(Collectors.toList())
            );
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public SaleChannelDTO update(RequestSaleChannel requestSaleChannel) throws BadRequestExceptions {
        try {
            requestSaleChannel.setName(requestSaleChannel.getName().toUpperCase());

            SaleChannel updatedSaleChannel = saleChannelMapper.requestSaleChannelToSaleChannel(requestSaleChannel);
            updatedSaleChannel.setDateRegistration(new Date(System.currentTimeMillis()));
            SaleChannel saleChannel = saleChannelRepository.save(updatedSaleChannel);
            return saleChannelMapper.saleChannelToSaleChannelDTO(saleChannel);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            saleChannelRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            saleChannelRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<SaleChannelDTO> list() throws BadRequestExceptions{
        try {
            return saleChannelMapper.listSaleChannelToListSaleChannelDTO(saleChannelRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SaleChannelDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return saleChannelMapper.saleChannelToSaleChannelDTO(saleChannelRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SaleChannelDTO findByName(String name) throws BadRequestExceptions{
        try {
            return saleChannelMapper.saleChannelToSaleChannelDTO(saleChannelRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
