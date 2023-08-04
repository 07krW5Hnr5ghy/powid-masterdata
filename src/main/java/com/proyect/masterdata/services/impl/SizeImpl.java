package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.SizeMapper;
import com.proyect.masterdata.repository.SizeRepository;
import com.proyect.masterdata.services.IMasterList;
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class SizeImpl implements ISize {
    private final SizeRepository sizeRepository;
    private final SizeMapper sizeMapper;

    @Override
    public ResponseSuccess save(String name) throws BadRequestExceptions {
        try {
            sizeRepository.save(sizeMapper.sizeToName(name.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names) throws BadRequestExceptions{
        try {
            sizeRepository.saveAll(sizeMapper.listSizeToListName(
                    names.stream().map(String::toUpperCase).collect(Collectors.toList())));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public SizeDTO update(RequestSize requestSize) throws BadRequestExceptions {
        try {
            requestSize.setName(requestSize.getName().toUpperCase());
            Size size = sizeRepository.save(sizeMapper.requestSizeToSize(requestSize));
            return sizeMapper.sizeToSizeDTO(size);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            sizeRepository.deleteById(code);
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
            sizeRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<SizeDTO> list() throws BadRequestExceptions{
        try {
            return sizeMapper.listSizeToListSizeDTO(sizeRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return sizeMapper.sizeToSizeDTO(sizeRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public SizeDTO findByName(String name) throws BadRequestExceptions{
        try {
            return sizeMapper.sizeToSizeDTO(sizeRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
