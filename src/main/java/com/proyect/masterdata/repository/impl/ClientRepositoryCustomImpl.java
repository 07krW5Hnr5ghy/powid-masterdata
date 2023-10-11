package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.repository.ClientRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class ClientRepositoryCustomImpl implements ClientRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Client> searchForClient(
            String ruc,
            String business,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Long status
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Client> criteriaQuery = criteriaBuilder.createQuery(Client.class);
        Root<Client> itemRoot = criteriaQuery.from(Client.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(ruc,business,user,status,criteriaBuilder,itemRoot);

        if(!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> clientList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                clientList = listASC(sortColumn,criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                clientList = listDESC(sortColumn,criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(clientList);
        }else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<Client> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber,pageSize);
        long count = getOrderCount(ruc,business,user,status);
        return new PageImpl<>(orderTypedQuery.getResultList(),pageable,count);
    }

    public List<Predicate> predicateConditions(
            String ruc,
            String business,
            String user,
            Long status,
            CriteriaBuilder criteriaBuilder,
            Root<Client> itemRoot
    ){
        List<Predicate> conditions = new ArrayList<>();
        if(ruc!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("ruc")),ruc.toUpperCase()
                            )
                    )
            );
        }
        if(business!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("business")),business.toUpperCase()
                            )
                    )
            );
        }
        if(user!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("user")),business.toUpperCase()
                            )
                    )
            );
        }
        if(status!=null){
            conditions.add(criteriaBuilder.equal(itemRoot.get("status"),status));
        }
        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Client> itemRoot
    ){
        List<Order> clientList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("business")){
            clientList.add(criteriaBuilder.asc(itemRoot.get("business")));
        }
        return clientList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Client> itemRoot
    ){
        List<Order> clientList = new ArrayList<>();
        if(sortColumn.equalsIgnoreCase("business")){
            clientList.add(criteriaBuilder.desc(itemRoot.get("business")));
        }
        return clientList;
    }

    private long getOrderCount(String ruc,String business,String user,Long status){
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Client> itemRoot = criteriaQuery.from(Client.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(ruc,business,user,status,criteriaBuilder,itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
