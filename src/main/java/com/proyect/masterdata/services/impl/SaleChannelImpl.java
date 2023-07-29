package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SaleChannelMapper;
import com.proyect.masterdata.repository.SaleChannelRepository;
import com.proyect.masterdata.services.IMasterList;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class SaleChannelImpl implements IMasterList {
    private final SaleChannelRepository saleChannelRepository;
    private final SaleChannelMapper saleChannelMapper;

    @Override
    public List<MasterListDTO> listRecords() throws BadRequestExceptions {
        return saleChannelMapper.saleChannelListToSaleChannelListDTO(saleChannelRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name) throws BadRequestExceptions {
        try{
            saleChannelRepository.save(SaleChannel.builder().name(name).status(true).build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch(RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            SaleChannel saleChannel = saleChannelRepository.findById(id).get();
            saleChannelRepository.save(SaleChannel.builder()
                    .name(saleChannel.getName())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(saleChannel.getId())
                    .status(false)
                    .build());
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public MasterListDTO updateRecord(String name, Long id) throws BadRequestExceptions {
        try{
            SaleChannel saleChannel = saleChannelRepository.save(SaleChannel.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .status(true)
                    .build()
            );
            return saleChannelMapper.INSTANCE.saleChannelToSaleChannelDTO(saleChannel);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
