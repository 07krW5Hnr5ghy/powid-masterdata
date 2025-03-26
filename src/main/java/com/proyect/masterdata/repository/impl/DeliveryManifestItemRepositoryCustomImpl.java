package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.DeliveryManifestItemRepositoryCustom;
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

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class DeliveryManifestItemRepositoryCustomImpl implements DeliveryManifestItemRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<DeliveryManifestItem> searchForDeliveryManifestItem(
            UUID clientId,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<DeliveryManifestItem> criteriaQuery = criteriaBuilder.createQuery(DeliveryManifestItem.class);
        Root<DeliveryManifestItem> itemRoot = criteriaQuery.from(DeliveryManifestItem.class);
        Join<DeliveryManifestItem, Product> deliveryManifestItemProductJoin = itemRoot.join("product");
        Join<DeliveryManifestItem, DeliveryManifest> deliveryManifestItemDeliveryManifestJoin = itemRoot.join("deliveryManifest");
        Join<DeliveryManifestItem, User> deliveryManifestItemUserJoin = itemRoot.join("user");
        Join<DeliveryManifestItem,OrderItem> deliveryManifestItemOrderItemJoin = itemRoot.join("orderItem");
        Join<Product,Color> productColorJoin = deliveryManifestItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = deliveryManifestItemProductJoin.join("size");
        Join<Product,Model> productModelJoin = deliveryManifestItemProductJoin.join("model");
        Join<Model,Brand> modelBrandJoin = productModelJoin.join("brand");
        Join<OrderItem,Ordering> orderItemOrderingJoin = deliveryManifestItemOrderItemJoin.join("ordering");
        Join<DeliveryManifest,Courier> deliveryManifestCourierJoin = deliveryManifestItemDeliveryManifestJoin.join("courier");
        Join<DeliveryManifest,Warehouse> deliveryManifestWarehouseJoin = deliveryManifestItemDeliveryManifestJoin.join("warehouse");
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
              clientId,
              quantity,
              collected,
              orderNumber,
              manifestNumber,
              color,
              size,
              model,
              brand,
              delivered,
              courier,
              courierDni,
              warehouse,
              registrationStartDate,
              registrationEndDate,
              updateStartDate,
              updateEndDate,
              criteriaBuilder,
              itemRoot,
              deliveryManifestItemUserJoin,
              orderItemOrderingJoin,
              deliveryManifestItemDeliveryManifestJoin,
              productColorJoin,
              productSizeJoin,
              productModelJoin,
              modelBrandJoin,
              deliveryManifestCourierJoin,
              deliveryManifestWarehouseJoin
        );
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> deliveryManifestList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                deliveryManifestList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                deliveryManifestList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(deliveryManifestList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }
        TypedQuery<DeliveryManifestItem> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);
        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getDeliveryManifestItemCount(
                clientId,
                quantity,
                collected,
                orderNumber,
                manifestNumber,
                color,
                size,
                model,
                brand,
                delivered,
                courier,
                courierDni,
                warehouse,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate
        );
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicate(
            UUID clientId,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<DeliveryManifestItem> itemRoot,
            Join<DeliveryManifestItem,User> deliveryManifestItemUserJoin,
            Join<OrderItem,Ordering> orderItemOrderingJoin,
            Join<DeliveryManifestItem,DeliveryManifest> deliveryManifestItemDeliveryManifestJoin,
            Join<Product,Color> productColorJoin,
            Join<Product,Size> productSizeJoin,
            Join<Product,Model> productModelJoin,
            Join<Model,Brand> modelBrandJoin,
            Join<DeliveryManifest,Courier> deliveryManifestCourierJoin,
            Join<DeliveryManifest,Warehouse> deliveryManifestWarehouseJoin
    ){
        List<Predicate> conditions = new ArrayList<>();
        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(deliveryManifestItemUserJoin.get("clientId"), clientId)));
        }
        if(quantity!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }
        if(collected!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("collected"), collected)));
        }
        if(orderNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(orderItemOrderingJoin.get("orderNumber"),orderNumber)));
        }
        if(manifestNumber!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(deliveryManifestItemDeliveryManifestJoin.get("manifestNumber"),manifestNumber)));
        }
        if(color!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }
        if(size!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }
        if(model!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }
        if(brand!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(modelBrandJoin.get("name")),"%"+brand.toUpperCase()+"%"));
        }
        if(Boolean.TRUE.equals(delivered)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("delivered"))));
        }
        if(Boolean.FALSE.equals(delivered)){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("delivered"))));
        }
        if(courier != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryManifestCourierJoin.get("name")),"%"+courier.toUpperCase()+"%"));
        }
        if(courierDni != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(deliveryManifestCourierJoin.get("dni"), courierDni)));
        }
        if(warehouse!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(deliveryManifestWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }
        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }
        return conditions;
    };

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryManifestItem> itemRoot) {

        List<Order> deliveryManifesItemtList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryManifesItemtList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return deliveryManifesItemtList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<DeliveryManifestItem> itemRoot) {

        List<Order> deliveryManifesItemtList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            deliveryManifesItemtList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            deliveryManifesItemtList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return deliveryManifesItemtList;

    }
    private Long getDeliveryManifestItemCount(
            UUID clientId,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<DeliveryManifestItem> itemRoot = criteriaQuery.from(DeliveryManifestItem.class);
        Join<DeliveryManifestItem, Product> deliveryManifestItemProductJoin = itemRoot.join("product");
        Join<DeliveryManifestItem, DeliveryManifest> deliveryManifestItemDeliveryManifestJoin = itemRoot.join("deliveryManifest");
        Join<DeliveryManifestItem, User> deliveryManifestItemUserJoin = itemRoot.join("user");
        Join<DeliveryManifestItem,OrderItem> deliveryManifestItemOrderItemJoin = itemRoot.join("orderItem");
        Join<Product,Color> productColorJoin = deliveryManifestItemProductJoin.join("color");
        Join<Product,Size> productSizeJoin = deliveryManifestItemProductJoin.join("size");
        Join<Product,Model> productModelJoin = deliveryManifestItemProductJoin.join("model");
        Join<Model,Brand> modelBrandJoin = productModelJoin.join("brand");
        Join<OrderItem,Ordering> orderItemOrderingJoin = deliveryManifestItemOrderItemJoin.join("ordering");
        Join<DeliveryManifest,Courier> deliveryManifestCourierJoin = deliveryManifestItemDeliveryManifestJoin.join("courier");
        Join<DeliveryManifest,Warehouse> deliveryManifestWarehouseJoin = deliveryManifestItemDeliveryManifestJoin.join("warehouse");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                quantity,
                collected,
                orderNumber,
                manifestNumber,
                color,
                size,
                model,
                brand,
                delivered,
                courier,
                courierDni,
                warehouse,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                deliveryManifestItemUserJoin,
                orderItemOrderingJoin,
                deliveryManifestItemDeliveryManifestJoin,
                productColorJoin,
                productSizeJoin,
                productModelJoin,
                modelBrandJoin,
                deliveryManifestCourierJoin,
                deliveryManifestWarehouseJoin
        );
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
